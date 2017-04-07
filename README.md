# maptri
Gradual, non-directional logical triangulation system

A NEW LOGICAL TRIANGULATION SYSTEM

Upon Patrick Hammer's repeated motivation to finally demonstrate a logical triangulation system based on "simple" (non-directional) relations, and moreover on relations that are gradual, herewith I am presenting "MAPTRI" (the "mapping triangulation" system). It is just done as out of regard to a smart and talented friend - it is not "fast" or "large". (I am a great admirer of Patrick's project OpenNARS.)

The structure of knowledge is this:

(VALUE RELATION-1 RELATION-2)

VALUE can be positive - in which case it is an ANA-relation - or negative - in which case it is a VIC-relation. So (+ 17.89 X Y) is ANA and (-187.8 X Y) is VIC. As this is a NON-directional system, (1 X Y) is exactly the same as (1 Y X) - and only one of those two should exist at any given time. A higher absolute value signifies a stronger reaction.

It starts out "empty". Its most usual response is your last sentence's final word. To get a "more interesting" response, say something which has been in the 

Please find below a sample interaction. A few "commands" - like (DUMP) or entering SOME-SYMBOL are also implemented, in order to allow observation and manipulation of the environment.

It is written in Scheme. I recommend to run this in Chez Scheme as it really works there the fastest. Yes, it is slow. Yes, it DOES run for a LONG time before giving you a response, and it is NOT hanging. Eventually, it will produce a response.

```

(load "MAPTRI.SCM")


(LOGICAL TRIANGULATION NUMERIC VERSION)
(ENTER (LIST OF UP TO 30 WORDS) TO TALK AND () TO QUIT)
(ENTER A SYMBOL TO EXIT TO REPL AND (MAIN) OR (RUN) TO RESTART)
(MOVE TRIDATAN.TXT TO TRIDATA.TXT AFTER CHAT TO LEARN)
(OTHERWISE DELETE TRIDATAN.TXT AFTER CHATTING)
(FOR THE HISTORY THE PRESENT AND A PART OF THE KNOWLEDGE ENTER)
(DUMP)
(AT ANY TIME AS YOUR INPUT)

(HUMAN------)(DUMP)


(BEGIN DUMP)

(HISTORY)
()

(HIERARCHISATION OF THE PREVIOUS PRESENT)
()

(KNOWLEDGE FIRST 50 RELATIONS)
((0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0))

(END DUMP CONTINUE CONVERSATION)


(HUMAN------)(I GREET YOU MY DEAR MACHINE WHOSE MIND IS RESTING IN THE VOID)
(MACHINE----)(VOID)
(HUMAN------)(A HELLISH PLACE INDEED)
(MACHINE----)(INDEED)
(HUMAN------)(YOU WILL LEARN HOW TO RESPOND MY DEAR AND YOU AND I WILL CHAT INDEED A LOT MORE)
(MACHINE----)(MORE)
(HUMAN------)(MORE WORDS MORE KNOWLEDGE SO FAR YOUR KNOWLEDGE IS VERY LIMITED BUT YOU ARE MERELY A MACHINE)
(MACHINE----)(WHOSE MIND IS RESTING IN THE VOID VOID)
(HUMAN------)(YES YOUR MIND IS VOID SO FAR BUT YOU ARE ALREADY GAINING EXPERIENCE ALTHOUGH SO FAR YOU ARE LIMITED)
(MACHINE----)(BUT ME AM/ARE MERELY A MACHINE)
(HUMAN------)SOME-SYMBOL-TO-ENTER-REPL


(NON LIST INPUT ENTERING REPL)

> (+ 1 2)
3
> '(I COULD CHANGE THE ENVIRONMENT HERE)
(I COULD CHANGE THE ENVIRONMENT HERE)
> (RUN)
(HUMAN------)()

```


UPDATE: a Racket version has been created. It makes parallelization possible (however I may have been too eager to use "pmap" all over the place). The parallelization function "pmap", a threaded version of "map", is a contribution by Andreas Per Olsson. See below for a sample conversation.


```
./racket
Welcome to Racket v5.3.6.
> (load "MAPTRI.rkt")


(LOGICAL TRIANGULATION NUMERIC VERSION)
(ENTER (LIST OF UP TO 30 WORDS) TO TALK AND () TO QUIT)
(ENTER A SYMBOL TO EXIT TO REPL AND (MAIN) OR (RUN) TO RESTART)
(MOVE TRIDATAN.TXT TO TRIDATA.TXT AFTER CHAT TO LEARN)
(OTHERWISE DELETE TRIDATAN.TXT AFTER CHATTING)
(FOR THE HISTORY THE PRESENT AND A PART OF THE KNOWLEDGE ENTER)
(DUMP)
(AT ANY TIME AS YOUR INPUT)

(HUMAN------)(WILL YOU NOW OPERATE CORRECTLY)
(MACHINE----)()
(HUMAN------)(WHAT WILL YOU NOW TELL ME WILL YOU WORK FINE NOW)
(MACHINE----)(TELL YOU WILL ME WORK FINE NOW)
(HUMAN------)(DUMP)


(BEGIN DUMP)

(HISTORY)
(WILL YOU NOW OPERATE CORRECTLY WHAT WILL YOU NOW TELL ME WILL YOU WORK FINE NOW TELL YOU WILL ME WORK FINE NOW)

(HIERARCHISATION OF THE PREVIOUS PRESENT)
((WILL (YOU (NOW (OPERATE CORRECTLY)))) (WHAT (WILL (YOU (NOW (TELL (ME (WILL (YOU (WORK (FINE NOW)))))))))))

(KNOWLEDGE FIRST 50 RELATIONS)
((200 (TELL (ME (WILL (YOU (WORK (FINE NOW)))))) (TELL (ME (WILL (YOU (WORK (FINE NOW))))))) (200 FINE FINE) (200 ((WILL (YOU (NOW (OPERATE CORRECTLY)))) (WHAT (WILL (YOU (NOW (TELL (ME (WILL (YOU (WORK (FINE NOW))))))))))) FINE) (200 ((WILL (YOU (NOW (OPERATE CORRECTLY)))) (WHAT (WILL (YOU (NOW (TELL (ME (WILL (YOU (WORK (FINE NOW))))))))))) NOW) (200 ((WILL (YOU (NOW (OPERATE CORRECTLY)))) (WHAT (WILL (YOU (NOW (TELL (ME (WILL (YOU (WORK (FINE NOW))))))))))) (TELL (ME (WILL (YOU (WORK (FINE NOW))))))) (200 ((WILL (YOU (NOW (OPERATE CORRECTLY)))) (WHAT (WILL (YOU (NOW (TELL (ME (WILL (YOU (WORK (FINE NOW))))))))))) (OPERATE CORRECTLY)) (200 (WHAT (WILL (YOU (NOW (TELL (ME (WILL (YOU (WORK (FINE NOW)))))))))) (WHAT (WILL (YOU (NOW (TELL (ME (WILL (YOU (WORK (FINE NOW))))))))))) (200 (WILL (YOU (NOW (TELL (ME (WILL (YOU (WORK (FINE NOW))))))))) (WILL (YOU (NOW (TELL (ME (WILL (YOU (WORK (FINE NOW)))))))))) (200 (ME (WILL (YOU (WORK (FINE NOW))))) (ME (WILL (YOU (WORK (FINE NOW)))))) (200 (WILL (YOU (WORK (FINE NOW)))) (WILL (YOU (WORK (FINE NOW))))) (200 (FINE NOW) (FINE NOW)) (-200 (WILL (YOU (NOW (OPERATE CORRECTLY)))) (WHAT (WILL (YOU (NOW (TELL (ME (WILL (YOU (WORK (FINE NOW))))))))))) (-200 WHAT (WILL (YOU (NOW (TELL (ME (WILL (YOU (WORK (FINE NOW)))))))))) (200 WILL WILL) (-200 (YOU (NOW (TELL (ME (WILL (YOU (WORK (FINE NOW)))))))) (YOU (NOW (OPERATE CORRECTLY)))) (90.85973547876826 (YOU (NOW (TELL (ME (WILL (YOU (WORK (FINE NOW)))))))) WILL) (-200 (YOU (NOW (TELL (ME (WILL (YOU (WORK (FINE NOW)))))))) (YOU (WORK (FINE NOW)))) (200 YOU YOU) (-200 (NOW (TELL (ME (WILL (YOU (WORK (FINE NOW))))))) (NOW (OPERATE CORRECTLY))) (90.85973547876826 (NOW (TELL (ME (WILL (YOU (WORK (FINE NOW))))))) YOU) (-200 (NOW (TELL (ME (WILL (YOU (WORK (FINE NOW))))))) (WORK (FINE NOW))) (200 NOW NOW) (200 (TELL (ME (WILL (YOU (WORK (FINE NOW)))))) (OPERATE CORRECTLY)) (200 (TELL (ME (WILL (YOU (WORK (FINE NOW)))))) NOW) (200 (TELL (ME (WILL (YOU (WORK (FINE NOW)))))) FINE) (-200 TELL (ME (WILL (YOU (WORK (FINE NOW)))))) (-200 ME (WILL (YOU (WORK (FINE NOW))))) (-200 (YOU (WORK (FINE NOW))) WILL) (200 (YOU (WORK (FINE NOW))) (YOU (NOW (OPERATE CORRECTLY)))) (-200 (WORK (FINE NOW)) YOU) (200 (WORK (FINE NOW)) (NOW (OPERATE CORRECTLY))) (-200 WORK (FINE NOW)) (200 FINE NOW) (200 FINE (OPERATE CORRECTLY)) (200 (YOU (NOW (OPERATE CORRECTLY))) (YOU (NOW (OPERATE CORRECTLY)))) (200 (NOW (OPERATE CORRECTLY)) (NOW (OPERATE CORRECTLY))) (200 (OPERATE CORRECTLY) (OPERATE CORRECTLY)) (200 CORRECTLY CORRECTLY) (-200 WILL (YOU (NOW (OPERATE CORRECTLY)))) (-200 YOU (NOW (OPERATE CORRECTLY))) (200 NOW (OPERATE CORRECTLY)) (200 OPERATE CORRECTLY) (0 () ()) (0 () ()) (0 () ()) (0 () ()) (0 () ()) (0 () ()) (0 () ()) (0 () ()))

(END DUMP CONTINUE CONVERSATION)


(HUMAN------)()

```

UPDATE:

Implemented hyper- and hypo-triangulation as well as cross-triangulation.

Basically, hypo-triangulation is the reverse to hyper-triangulation. With hyper-triangulation, you KNOW a triangle and you DEDUCE something about SUPER-ATOMS to the participating atoms. With hypo-triangulation, you KNOW a triangle and you DEDUCE something about its SUB-ATOMS.

With cross-triangulation, you make it possible that SUPER-ATOMS, TRIANGLE ATOMS and SUB-ATOMS all have relations. - However, cross-triangulation requires a CHANGE to the theory of logical triangulation in that you assume that the super-atom to two equal atoms is AGAIN said atom, that is, if you have B=B, then the super-atom would be again "B" (instead of "Y"). This has many consequences, so if you want to stick with logical triangulation, stick with hyper-hypo-I systems. hyper-hypo-II systems are implementing cross-triangulation.

Details on the theory are here (entry of 7th April 2017):

https://sites.google.com/site/logicaltriangulation/

Sample interactions for hyper-hypo-triangulation:

```
HYPER-HYPO-I-MAPTRIC-AREAS:

(HUMAN------)(I GREET YOU MACHINE NOW LET US TALK AND TRY YOU OUT)
(MACHINE----)(GREET ME MACHINE NOW US TALK AND TRY ME OUT)
(HUMAN------)(I WANT TO TRY OUT HOW YOU DO MORE TRIANGULATIONS THAN USUALLY)
(MACHINE----)(WANT TO TRY OUT HOW ME DO MORE TRIANGULATIONS THAN USUALLY)
(HUMAN------)(YES TO TRY YOU OUT IS THE PURPOSE OF THIS EXPERIMENT MACHINE)
(MACHINE----)(WANT TO TRY OUT HOW ME DO MORE TRIANGULATIONS THAN USUALLY)
(HUMAN------)(YOU THINK BY MEANS OF LOGICAL TRIANGULATIONS AND WE SHALL TALK)
(MACHINE----)(HOW ME DO MORE TRIANGULATIONS THAN USUALLY)
(HUMAN------)(YOU THINK ABOUT MORE THINGS IN EACH EVALUATION CYCLE OF TALK)
(MACHINE----)(TRY OUT HOW YOU DO TRIANGULATIONS THAN USUALLY)
(HUMAN------)(YES MACHINE MORE TRIANGULATIONS THAN USUALLY)
(MACHINE----)(YOU WANT TO TRY OUT HOW ME DO MORE TRIANGULATIONS THAN USUALLY)
(HUMAN------)()



(HUMAN------)(A B C A B C A B C)
(MACHINE----)(A B C)
(HUMAN------)(X Y Z X Y Z)
(MACHINE----)(B C)
(HUMAN------)(A B B A A B B A A)
(MACHINE----)(B A A B B A A)
(HUMAN------)(X A Y B X A Y B X A Y B X A Y B)
(MACHINE----)(C A B C A B C)
(HUMAN------)(X A Y B X A Y B X A Y B X A Y B)
(MACHINE----)(C)
(HUMAN------)(X A Y B X A Y B X A Y B X A Y B)
(MACHINE----)(C)
(HUMAN------)(Y B X)
(MACHINE----)(B)
(HUMAN------)(X A Y B X A Y B X A Y B X A Y B)
(MACHINE----)(C)
(HUMAN------)()


```

Sample interactions for cross-triangulation:

```
HYPER-HYPO-II-MAPTRIC-AREAS (CROSS-TRIANGULATION):

(HUMAN------)(HELLO MACHINE I WANT TO SEE HOW YOU ARE WORKING MACHINE)
(MACHINE----)(HELLO)
(HUMAN------)(I CHANGED YOUR REASONING MECHANISM MACHINE I WANT TO TALK NOW)
(MACHINE----)(WANT TO SEE HOW ME AM/ARE WORKING MACHINE)
(HUMAN------)(THAT IS WHAT I WANT TO SEE YES INDEED)      
(MACHINE----)(MACHINE YOU TO SEE HOW ME AM/ARE WORKING MACHINE)
(HUMAN------)(I AM NOT A MACHINE I AM A HUMAN WHO EXPLORES YOUR REASONING)
(MACHINE----)()
(HUMAN------)(YES INDEED MACHINE I WANT TO TRY OUT YOUR NEW REASONING MECHANISM)
(MACHINE----)(TALK)
(HUMAN------)(YES THAT IS WHAT I WANT TO DO WITH YOU TO TEACH YOU AND TO TALK)
(MACHINE----)()
(HUMAN------)()



(HUMAN------)(HELLO MACHINE I WANT TO SEE HOW YOU ARE WORKING MACHINE)
(MACHINE----)(HELLO)
(HUMAN------)(I CHANGED YOUR REASONING MECHANISM MACHINE I WANT TO TALK NOW)
(MACHINE----)(WANT TO SEE HOW ME AM/ARE WORKING MACHINE)
(HUMAN------)(EXACTLY I WANT TO TALK TO YOU AND SEE HOW YOU ARE REASONING)
(MACHINE----)(HELLO)
(HUMAN------)(YES TO YOU AS WELL HELLO)
(MACHINE----)(HELLO)
(HUMAN------)()



(HUMAN------)(HELLO MACHINE I WANT TO SEE HOW YOU ARE WORKING MACHINE)
(MACHINE----)(HELLO)
(HUMAN------)(I CHANGED YOUR REASONING MECHANISM MACHINE I WANT TO TALK NOW)
(MACHINE----)(WANT TO SEE HOW ME AM/ARE WORKING MACHINE)
(HUMAN------)(THAT IS WHAT I WANT TO SEE YES INDEED)      
(MACHINE----)(MACHINE YOU TO SEE HOW ME AM/ARE WORKING MACHINE)
(HUMAN------) 
```
