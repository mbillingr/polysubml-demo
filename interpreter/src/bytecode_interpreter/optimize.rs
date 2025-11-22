use crate::bytecode_interpreter::compiler::Op;
use std::collections::{BTreeSet, HashSet};
use std::rc::Rc;

/// Optimize a block of code.
pub fn optimize(ops: Vec<Op>) -> Vec<Op> {
    let ops = optimize_functions(ops);
    let ops = reduce_jump_chains(ops);

    let bbs = basic_blocks(ops);
    let bbs = bbs.into_iter().map(peephole);
    let ops = recombine(bbs);

    ops
}

/// Optimize all function bodies in a block of code.
fn optimize_functions(mut ops: Vec<Op>) -> Vec<Op> {
    for op in &mut ops {
        match op {
            Op::MakeClosure(body) => *body = Rc::new(optimize((**body).clone())),
            _ => {}
        }
    }
    ops
}

/// Put basic blocks back together
fn recombine(bbs: impl IntoIterator<Item = BasicBlock>) -> Vec<Op> {
    let mut ops = vec![];
    for bb in bbs {
        ops.extend(bb.0);
    }
    ops
}

/// Split a block of code into basic blocks.
fn basic_blocks(ops: Vec<Op>) -> Vec<BasicBlock> {
    let mut block_cuts = BTreeSet::new();
    block_cuts.insert(0);
    block_cuts.insert(ops.len());
    for (i, op) in ops.iter().enumerate() {
        if is_exit(op) {
            block_cuts.insert(i + 1);
        }
        if let Some(offset) = get_offset(op) {
            block_cuts.insert((i as isize + offset + 1) as usize);
        }
    }

    let mut jump_targets = block_cuts.into_iter();
    let mut start = jump_targets.next().unwrap();
    let mut bbs = vec![];
    for end in jump_targets {
        bbs.push(BasicBlock::new(ops[start..end].to_vec()));
        start = end;
    }
    bbs
}

#[derive(Debug)]
struct BasicBlock(Vec<Op>);

impl BasicBlock {
    fn new(ops: Vec<Op>) -> Self {
        debug_assert!(!ops.iter().rev().skip(1).any(is_exit));
        Self(ops)
    }
}

fn is_exit(op: &Op) -> bool {
    matches!(
        op,
        Op::Return | Op::Jump(_) | Op::JumpWhenFalse(_) | Op::JumpAndPopWhenTag(_, _) | Op::PeekAndJumpNotTag(_, _)
    )
}

/// Peephole optimization of a basic block
fn peephole(BasicBlock(ops): BasicBlock) -> BasicBlock {
    let mut out = vec![];
    for op in ops {
        out.push(op);

        loop {
            match out.as_slice() {
                // if a variable is loaded twice, load it only once and duplicate the value
                [.., Op::PushVar(a), Op::PushVar(b)] if a == b => {
                    out.pop().unwrap();
                    out.push(Op::Dup)
                }

                _ => break,
            }
        }
    }
    BasicBlock::new(out)
}

/// Optimize chains of unconditional jumps to a single instruction.
fn reduce_jump_chains(mut ops: Vec<Op>) -> Vec<Op> {
    let mut changes = true;
    while changes {
        changes = false;
        for i in 0..ops.len() as isize {
            changes |= get_offset(&mut ops[i as usize])
                .map(|offset| find_final_jump_target(&ops, i + offset + 1))
                .map(|jt| jt.update(i, &mut ops))
                .unwrap_or(false);
        }
    }
    ops
}

fn get_offset(op: &Op) -> Option<isize> {
    match op {
        Op::Jump(offset) => Some(*offset),
        Op::JumpWhenFalse(offset) => Some(*offset),
        Op::PeekAndJumpNotTag(_, offset) => Some(*offset),
        Op::JumpAndPopWhenTag(_, offset) => Some(*offset),
        _ => None,
    }
}

fn get_offset_mut(op: &mut Op) -> Option<&mut isize> {
    match op {
        Op::Jump(offset) => Some(offset),
        Op::JumpWhenFalse(offset) => Some(offset),
        Op::PeekAndJumpNotTag(_, offset) => Some(offset),
        Op::JumpAndPopWhenTag(_, offset) => Some(offset),
        _ => None,
    }
}

fn find_final_jump_target(ops: &[Op], mut pos: isize) -> JumpTarget {
    let mut visited = HashSet::new();
    loop {
        if visited.contains(&pos) {
            return JumpTarget::Never;
        }
        visited.insert(pos);

        match &ops[pos as usize] {
            Op::Jump(-1) => return JumpTarget::Never,
            Op::Jump(offset) => pos = pos + offset + 1,
            Op::Return => return JumpTarget::Return,
            _ => return JumpTarget::JumpTo(pos),
        }
    }
}

#[derive(Debug, PartialEq)]
enum JumpTarget {
    /// Sequence of jumps cycles back to itself; it never finishes
    Never,
    /// Sequence of jumps ends at a return instruction
    Return,
    /// Sequence of jumps ends at an arbitrary instruction
    JumpTo(isize),
}

impl JumpTarget {
    fn update(&self, pos: isize, op: &mut [Op]) -> bool {
        match self {
            JumpTarget::Never => match &mut op[pos as usize] {
                Op::Jump(-1) => false,
                Op::Jump(offset) => {
                    *offset = -1;
                    true
                }
                _ => false,
            },

            JumpTarget::Return => {
                if let Op::Jump(_) = op[pos as usize] {
                    op[pos as usize] = Op::Return;
                    true
                } else {
                    false
                }
            }

            JumpTarget::JumpTo(target) => {
                let offset = match get_offset_mut(&mut op[pos as usize]) {
                    Some(offset) => offset,
                    None => return false,
                };
                let new_offset = target - pos - 1;
                if new_offset != *offset {
                    *offset = new_offset;
                    true
                } else {
                    false
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_find_final_jump_target() {
        assert_eq!(find_final_jump_target(&[Op::Jump(-1)], 0), JumpTarget::Never);
        assert_eq!(find_final_jump_target(&[Op::Jump(0), Op::Jump(-2)], 0), JumpTarget::Never);
        assert_eq!(find_final_jump_target(&[Op::Jump(0), Op::Jump(-2)], 1), JumpTarget::Never);
        assert_eq!(
            find_final_jump_target(&[Op::Jump(0), Op::Jump(0), Op::Jump(-2)], 0),
            JumpTarget::Never
        );
        assert_eq!(
            find_final_jump_target(&[Op::Jump(1), Op::Jump(0), Op::Jump(-2)], 0),
            JumpTarget::Never
        );

        assert_eq!(find_final_jump_target(&[Op::Return], 0), JumpTarget::Return);
        assert_eq!(find_final_jump_target(&[Op::Dup], 0), JumpTarget::JumpTo(0));

        assert_eq!(
            find_final_jump_target(&[Op::Jump(0), Op::Jump(0), Op::Return], 0),
            JumpTarget::Return
        );
        assert_eq!(
            find_final_jump_target(&[Op::Jump(0), Op::Jump(0), Op::Dup], 0),
            JumpTarget::JumpTo(2)
        );
        assert_eq!(
            find_final_jump_target(&[Op::Jump(1), Op::Return, Op::Jump(-2)], 0),
            JumpTarget::Return
        );
        assert_eq!(
            find_final_jump_target(&[Op::Jump(1), Op::Dup, Op::Jump(-2)], 0),
            JumpTarget::JumpTo(1)
        );

        assert_eq!(
            find_final_jump_target(&[Op::JumpWhenFalse(1), Op::Dup, Op::Jump(-2)], 0),
            JumpTarget::JumpTo(0)
        );
        assert_eq!(
            find_final_jump_target(&[Op::JumpWhenFalse(1), Op::Dup, Op::Jump(-2)], 2),
            JumpTarget::JumpTo(1)
        );
    }

    #[test]
    fn test_reduce_jump_chains() {
        assert_eq!(reduce_jump_chains(vec![Op::Return]), vec![Op::Return]);
        assert_eq!(reduce_jump_chains(vec![Op::Dup]), vec![Op::Dup]);

        assert_eq!(
            reduce_jump_chains(vec![Op::Jump(0), Op::Jump(0), Op::Return]),
            vec![Op::Return, Op::Return, Op::Return]
        );
        assert_eq!(
            reduce_jump_chains(vec![Op::Jump(0), Op::Jump(0), Op::Dup]),
            vec![Op::Jump(1), Op::Jump(0), Op::Dup]
        );
        assert_eq!(
            reduce_jump_chains(vec![Op::Jump(1), Op::Return, Op::Jump(-2)]),
            vec![Op::Return, Op::Return, Op::Return]
        );
        assert_eq!(
            reduce_jump_chains(vec![Op::Jump(1), Op::Dup, Op::Jump(-2)]),
            vec![Op::Jump(0), Op::Dup, Op::Jump(-2)]
        );

        assert_eq!(
            reduce_jump_chains(vec![Op::JumpWhenFalse(1), Op::Dup, Op::Jump(-2)]),
            vec![Op::JumpWhenFalse(0), Op::Dup, Op::Jump(-2)]
        );
        assert_eq!(
            reduce_jump_chains(vec![Op::JumpWhenFalse(1), Op::Return, Op::Jump(-2)]),
            vec![Op::JumpWhenFalse(1), Op::Return, Op::Return]
        );
        assert_eq!(
            reduce_jump_chains(vec![Op::Jump(1), Op::Dup, Op::JumpWhenFalse(-2)]),
            vec![Op::Jump(1), Op::Dup, Op::JumpWhenFalse(-2)]
        );
        assert_eq!(
            reduce_jump_chains(vec![Op::Jump(1), Op::Return, Op::JumpWhenFalse(-2)]),
            vec![Op::Jump(1), Op::Return, Op::JumpWhenFalse(-2)]
        );
    }

    #[test]
    fn test_reduce_infinite_jump_chains() {
        assert_eq!(reduce_jump_chains(vec![Op::Jump(-1)]), vec![Op::Jump(-1)]);
        assert_eq!(
            reduce_jump_chains(vec![Op::Jump(0), Op::Jump(-2)]),
            vec![Op::Jump(-1), Op::Jump(-1)]
        );
        assert_eq!(
            reduce_jump_chains(vec![Op::Jump(0), Op::Jump(0), Op::Jump(-2)]),
            vec![Op::Jump(-1), Op::Jump(-1), Op::Jump(-1)]
        );
        assert_eq!(
            reduce_jump_chains(vec![Op::Jump(1), Op::Jump(0), Op::Jump(-2)]),
            vec![Op::Jump(-1), Op::Jump(-1), Op::Jump(-1)]
        );
    }
}
