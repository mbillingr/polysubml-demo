let rec fizzbuzz = fun i -> (
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
  if i < 50 then
    fizzbuzz (i+1)
  else
    0
);
fizzbuzz 0;