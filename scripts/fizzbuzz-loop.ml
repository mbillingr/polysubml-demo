let vars = {mut i=0};
loop if vars.i >= 50 then `Break 0 else (
  let i = vars.i <- vars.i + 1;
  print (if i % 3 == 0 then
    if i % 5 == 0 then
      "FizzBuzz"
    else
      "Fizz"
  else
    if i % 5 == 0 then
      "Buzz"
    else
      i
  );
  `Continue 0
);