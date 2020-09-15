# LogPro-Interpreter
Interpreter for a toy logic programming language.
# Problem Statement:
In this assignment, you will write a simplified version of a Logic Programming interpreter in OCaml.

You will first define an ML data type to represent the structure of a legitimate LogPro program.

A program is a set (list) of clauses. 

 A clause can either be a fact or a rule. A fact has a head but no body.  A rule has a head and a body.  
 
The head is a single atomic formula.  A body is a sequence of atomic formulas.

An atomic formula is a k-ary predicate symbol followed by k terms.

A term is either a variable, a constant, or a k-ary function symbol with k sub-terms.

A goal is a set (list) of atomic formulas.

You need to take your implementation of unification to use as the parameter-passing mechanism. (Note: by pretending the predicate symbol is a function symbol, you can perform resolution of goals and program clauses).


You also need to develop a back-tracking strategy to explore the resolution search space.   You need to be able to replace a goal by subgoals, as found by applying a unifier to the body of a program clause whose head unified with the chosen subgoal.
_________________________________________________________________________________________________
___________________________________________________________________________________________________

To compile just do make
To run : ./assignment6 <filename_of_file_containing_knowledge_base>
and queries can be given input to stdin

Knowledge base containing all facts and rules is an array of maximum size 5000 size could be increased if required.

To exit query mode type "halt." (without quotes)

For queries with more than one solution : solutions are displayed one by one to view next solution press ; and enter

_____________________________________________________________________________________________________________________

First all of knowledge base is lexed and yacced (tokenizer.mll and parser.mly) and an array of facts and rules is created (createBase)
In the created database no two different clauses have same variable names , this is taken care by the fucntion rectify which names the variables as _Ui where i is a number. This function ensures that within a clause the the atoms with same variables still have the same variable names.

After database is created the queries/goals are taken as input from stdin and they are lexed and yaccedd (lexquery.mll and parsequery.mly) after this findAns function is called with argument as the query/goal (=atom list) which further calls for bactAtom function after this only the variables in the query/goals are printed with their respective solutions.

backAtom function is a bactracking algorithm which runs on an atom list finds solution to one atom(findAtom and mgu) and applies that to the remaining list .

