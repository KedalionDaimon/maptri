# maptri
Gradual, non-directional logical triangulation system

A NEW LOGICAL TRIANGULATION SYSTEM

Upon Patrick H.'s repeated motivation to finally demonstrate a logical triangulation system based on "simple" (non-directional) relations, and moreover on relations that are gradual, herewith I am presenting "MAPTRI" (the "mapping triangulation" system). It is just done as out of regard to a smart and talented friend - it is not "fast" or "large".

The structure of knowledge is this:

(VALUE RELATION-1 RELATION-2)

VALUE can be positive - in which case it is an ANA-relation - or negative - in which case it is a VIC-relation. So (+ 17.89 X Y) is ANA and (-187.8 X Y) is VIC. As this is a NON-directional system, (1 X Y) is exactly the same as (1 Y X) - and only one of those two should exist at any given time. A higher absolute value signifies a stronger reaction.

It starts out "empty". Its most usual response is your last sentence's final word. To get a "more interesting" response, say something which has been in the 

Please find below a sample interaction. A few "commands" - like (DUMP) or entering SOME-SYMBOL are also implemented, in order to allow observation and manipulation of the environment.

I recommend to run this in Chez Scheme as it really works there the fastest. Yes, it is slow. Yes, it DOES run for a LONG time before giving you a response, and it is NOT hanging. Eventually, it will produce a response.

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
