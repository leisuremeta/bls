## sbt project compiled with Scala 3

### small documentation

Original code: https://github.com/Chia-Network/bls-signatures
<br />
Goal: rewrite entire python implementation in scala 3
<br />
specs: https://www.ietf.org/archive/id/draft-irtf-cfrg-bls-signature-05.html
Specs

## What is the right way of rewriting the code base?
* Start with file with least dependencies to other files
* Check the testing file named imp-test.py from repo above and rewrite testing code blocks related to a file which you plan to implement. Check out test/scala/MySuite.scala for insights
* Start with satisfying type errors on console and gradually expand from there
* Run sbt on terminal and enter ~Test/compile to check correctness of your code
* If you see some errors then you should work on them.
* Fields.scala file is the longest and one of the hardest since it contains lots of type declarations and there you can witness clash of object oriented and functional programming paradigms.
* Hash_to_field.scala, hkdf.scala and large portion of fields.scala files were able to pass  ~Test/compile command. Other files are need modifications

