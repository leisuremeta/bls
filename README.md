## sbt project compiled with Scala 3

### small documentation

Original code: https://github.com/Chia-Network/bls-signatures
Goal: rewrite entire python implementation in scala 3
Specs

What is the right way of rewriting the code?
Start with file with least dependencies to other files
Check the testing file named imp-test.py from repo above and rewrite testing code blocks related to a file which you plan to implement. Check out test/scala/MySuite.scala for insights
Run sbt on terminal and enter ~Test/compile to check correctness of your code
If you see some errors then you should work on them.
Fields.scala file is the longest and one of the hardest since it contains lots of type declarations and there you can witness clash of object oriented and functional programming paradigms.
Hash_to_field.scala, hkdf.scala and partially fields.scala files were able to pass  ~Test/compile command. Other files are need modifications

