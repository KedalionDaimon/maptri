; #lang racket

(require racket/future)

(define (pmap func alist) ; Sort of pmap
  (let* ([f (for/list ([i alist]) (future (lambda () (func i ))) )])
    (for/list ([i f]) (touch i))))
; parallelization function above by Andreas Per Olsson - THANK YOU!

; everywhere where I use "map", this "map" could be
; theoretically parallelised - but I simply overdid it
; so to not forget where I used it, I am "marking down" map with "ppmap",
; though likely I should not have parallelized that:

(define (ppmap func lis) (map func lis))
; (define (ppmap func lis) (pmap func lis))
; - to make "all sort of stuff" parallel again.

; ======== EXPLANATION ========

; This is an implementation of a simple chatbot based on
; logical triangulation. I/O is done over lists - I did not
; bother with parsing. I tried to make this as portable as possible.

; This version is NOT strictly symbolic-oriented: "vic" and "ana"
; relations can have GRADUAL values and are not just "true" or
; "false". +VAL means ANA, -VAL means vic. A higher absolute value
; signifies a stronger relation.

; The structure of the relations is (VAL REL1 REL2).
; REL1 and REL2 are thereby NOT in any particular order.
; REL1 and REL2 can be set as complex tree-structures during reasoning.

; Triangulation is hence performed without any "coordinates".
; As vic and ana are expressed as val, there are no separate
; "vic-atoms" and "ana-atoms", either.
; Triangulation is subject to two limits:
; *limtri*
; is limiting the maximum count of triangles tolerated in each step.
; If more triangles are found, the remaining ones are not used to update the
; knowledge.
; *triangulation-count* is limiting how many "relations" may be followed.
; This encompasses potentially new conclusions.
; The maximum amount of triangles is thus
; (expt *limtri* *triangulation-count).
; This limit should normally not be reached at all.

; The RESULTS of triangulation are used in order to "hierarchise"
; (i.e. find a way to form higher-level structures within...) 
; the input substrate. I.e. how to turn (A B C D) e.g. into,
; finally, ((A B) (C D)).
; Hierarchisation is carried out from bottom to top, HOWEVER -
; only within the consideration window of *history-length*.
; Hierarchisation takes the "suggestions" from triangulation
; (or generally - knowledge), and uses the NON-DIRECTIONAL
; knowledge to perform a DIRECTIONAL structuring.
; i.e. (VAL A B) and (VAL B A) in KNOWLEDGE are ONE AND THE SAME
; - but (A B) and (B A) in HIERARCHISATION are entirely different
; atoms.
; Triangulation and knowledge ultimately rely on the information
; delivered through hierarchisation, so DIRECTIONAL hierarchisation
; thus still has an effect on knowledge:
; (VAL (X Y) Z) and (VAL (Y X) Z) are TWO ENTIRELY DIFFERENT RELATIONS;
; whereas (VAL (X Y) Z) and (VAL Z (X Y)) would be two expressions for
; ONE AND THE SAME relation.

; Triangulation and hierarchisation ultimately have one aim:
; to "generate a reply".
; To "understand" the world means to "structure" it - to
; discover the nature and strength of relations and to find
; new relations.
; After the relations have been established, then even without
; special effort, it should be known what "the reply" should be
; to "the present". If the present is X and the reply is Y,
; then you are merely looking for (VAL X Y). You then output
; Y as the "plan" or the system's reply. Y may be a complex
; tree-structure that first will need to be "flattened" for output.

; (A little detail:
; Unfortunately, (VAL X Y) is NON-directional, so we will need
; a "proof" that (VAL X Y) is NOT really Y->X.
; This proof is established if you can find either
; (VAL (X Y) Z) or (VAL Z (X Y)) - as here, (X Y) will be
; the result of hierarchisation, which has been transferred
; into knowledge, and hierarcisation IS "directional".)

; This version is using a lot of "map"-ing, hence the name.
; I expect Scheme in the future to facilitate some parallel
; way to do mapping.

; ======== VARIABLES ========

; how deep "tangential" considerations will the system make:
(define *triangulation-count* 2)
; 3 is the maximum to stay within the implementation limit for apply
; even in toy experiments.

; how much energy shall be given to a hypothesis upon perception:
(define *energy-token* 40)
; if the value is "low", then new perceptions will have a hard time
; influencing "established" knowledge, and vice versa
; it would be pointless to set this over *maxabs*

; what is the maximum tolerated absolute value:
(define *maxabs* 200)
; set this as high as you like, this is the maximum value of
; knowledge that is permitted

; what is the maximum amount of triangles tolerated per step:
(define *limtri* 30) ; 100)

; until what "depth" of knowledge are triangles sought:
(define *depth-of-short-term-memory* 30000) ; 300)

; how many words will be considered in the "history" of the present:
(define *history-length* 30) ; 50)

; auxiliary:
(define (mmb a b) (member a b))
; in CL, I have to adjust the equality test - to equal

; auxiliary:
; Common Lisp has this already
(define (butlast lis) (reverse (cdr (reverse lis))))

; auxiliary:
; (define (cdaddar x) (cdr (caddar x)))

; auxiliary:
(define (sign num) (cond ((< num 0) -1) (#t 1)))

; auxiliary:
; will be very useful after mapping functions:
(define (cleanse-from-null lis)
  (cond ((null? lis) '())
        ((null? (car lis)) (cleanse-from-null (cdr lis)))
        (#t (cons (car lis) (cleanse-from-null (cdr lis))))))
; (cleanse-from-null '(a b c () () d)) --> (a b c d)

; auxiliary:
; any plan will appear as a hierarchy - I must be able to decompose it
(define (flatten-list lis)
  (cond
    ((not (list? lis)) (list lis))
    ((null? lis) '())
    ((list? (car lis)) (flatten-list (append (car lis) (cdr lis))))
    (#t (cons (car lis) (flatten-list (cdr lis))))))
; (flatten-list '((a ((b c) (e f))))) --> (a b c e f)

; auxiliary:
; Gambit - test "apply append" implementation limit:
; (define (genlist count lis)
; (if (zero? count) lis (genlist (- count 1) (cons (list count) lis))))
; (apply append (genlist 4 '())) --> (1 2 3 4)
; (apply append (genlist 8192 '())) is the maximum that executed correctly
; This is all in all not too high.
; ; originally:
; (define (apply_append lists-to-be-appended)
;   (apply append lists-to-be-appended))
; now:
(define (apply_append lists-to-be-appended)
  (cond ((null? lists-to-be-appended) '())
        (#t (append (car lists-to-be-appended)
                    (apply_append (cdr lists-to-be-appended))))))

; auxiliary:
(define (proto-takefirst fromwhere howmany resultlist)
  (if (or (null? fromwhere) (zero? howmany)) (reverse resultlist)
    (proto-takefirst (cdr fromwhere) (- howmany 1)
      (cons (car fromwhere) resultlist))))

; auxiliary:
(define (takefirst fromwhere howmany)
  (proto-takefirst fromwhere howmany '()))

; auxiliary:
(define (takelast fromwhere howmany)
  (reverse (takefirst (reverse fromwhere) howmany)))

; auxiliary:
(define (takeafter fromwhere howmany)
  (if (or (null? fromwhere) (zero? howmany)) fromwhere
    (takeafter (cdr fromwhere) (- howmany 1))))

; this function shall prepare a potential input list
; as a series of vic-connections in order to check
; which ones would make good points of hierarchisation
(define (pair-up lis)
  (cond ((null? lis) '())
        ((null? (cdr lis)) (list (append lis lis)))
        (#t (map (lambda (x y) (list x y)) (butlast lis) (cdr lis)))))
; (pair-up '(a b c d)) --> ((a b) (b c) (c d))
; generally, this is idiotic, but should be foreseen:
; (pair-up '(a)) --> ((a a))

; the vic-candidates will have the form (a b),
; whereas the knowledge base will have the form (+-val a b),
; whereby -val or 0 means vic and +val means ana
; the knowledge shall be agnostic as to co-ordinates -
; co-ordinates will be expressed as separate atoms

; auxiliary - force finding ANY pair in the knowledge,
; even ana
(define (force-check-pair some-pair knowledge)
  (cond ((null? knowledge) '())
        ((equal? (cdar knowledge) some-pair)
            (car knowledge))
        ((equal? (cdar knowledge) (reverse some-pair))
            (car knowledge))
        (#t (force-check-pair some-pair (cdr knowledge)))))

; auxiliary - try to find a vic pair in the knowledge
(define (check-pair some-pair knowledge)
  (cond ((null? knowledge) '())
        ((and (equal? (cdar knowledge) some-pair)
          (>= 0 (caar knowledge)))
            (car knowledge))
        ((and (equal? (cdar knowledge) (reverse some-pair))
          (>= 0 (caar knowledge)))
            (car knowledge))
        (#t (check-pair some-pair (cdr knowledge)))))

; YOU COULD ALSO SWITCH THE BELOW LINE ON TO "FIND ANYTHING", NOT ONLY VICS:
; (define (check-pair some-pair knowledge)
;   (force-check-pair some-pair knowledge))

; find eventual atom pairs for all candidates coming from the input
; (define (find-potential-vics candidates knowledge)
;   (map (lambda (x) (check-pair x knowledge)) candidates))
; (find-potential-vics
; '((a b) (c d) (a b) (l m)) '((3 x y) (-7 b a) (-1 c d))) 
; --> ((-7 b a) (-1 c d) (-7 b a) ())
; note the changed direction of (a b)
;
; instead of the above, account for the possibility of NO match whatsoever
(define (find-potential-vics candidates knowledge)
  (let ((suggested-vics
          (cleanse-from-null
            (map (lambda (x) (check-pair x knowledge)) candidates))))
    (cond
; if all goes well, terminate straight away:
      ((not (null? suggested-vics)) suggested-vics)
; else force the last pair:
      (#t
; if there is an analogy, find it - forcing the last pair
; (alternative: look for the "least" analogous potential connection):
        (let ((force-pair
                (force-check-pair (car (reverse candidates)) knowledge)))
          (cond ((not (null? force-pair)) (list force-pair))
; else, create a hierarchisation by force from the last pair:
                (#t
; WAS PREVIOUSLY: ((((((( --- filling the parentheses matcher
;                 (list (append '(0) (car (reverse candidates)))))))))))
; now, an "act of despair" - "hierarchise anywhere, anything":
                  (let ((second-suggested-anas
                    (cleanse-from-null
                      (map (lambda (x)
                        (force-check-pair x knowledge)) candidates))))

                    (cond
; try now to terminate:
                      ((not (null? second-suggested-anas))
                        second-suggested-anas)
; else, finally, force the last pair:
                      (#t
                        (list
                         (append (list (- *energy-token*))
                                 (car (reverse candidates))))))))))))))
;                        (append '(0)
;                                (car (reverse candidates))))))))))))))

; (find-potential-vics '((c d)) '((3 x y) (7 b a) (-1 c d)))                
; --> ((-1 c d)) ; find existing vic
; (find-potential-vics '((a b)) '((3 x y) (7 b a) (-1 c d))) 
; --> ((7 b a)) ; find ana
; (find-potential-vics '((l m)) '((3 x y) (7 b a) (-1 c d))) 
; --> ((0 l m)) ; create new

; auxiliary - try to remove collisions
; of a candidate with other candidates
(define (remove-collisions candidate list-of-candidates)
  (cond
    ((null? list-of-candidates)
        candidate)
    ((or
      (null? (car list-of-candidates))
      (equal? (car list-of-candidates) candidate))
        (remove-collisions candidate (cdr list-of-candidates)))
    ((or
      (equal? (cadr candidate) (cadar list-of-candidates))
      (equal? (caddr candidate) (cadar list-of-candidates))
      (equal? (cadr candidate) (caddar list-of-candidates))
      (equal? (caddr candidate) (caddar list-of-candidates)))
        (cond ((>= (car candidate) (caar list-of-candidates))
                '())
              (#t
                (remove-collisions candidate (cdr list-of-candidates)))))
    (#t (remove-collisions candidate (cdr list-of-candidates)))))
; (remove-collisions '(-4 a b) '((-3 a x) (-1 b y) (8 r a) (0 s b)))       
; (-4 a b)
; (remove-collisions '(-4 a b) '((-3 a x) (-11 b y) (8 r a) (0 s b)))
; --> ()

; only collect the best and non-colliding vic-combinations
; for the next hierarchisation stage
(define (remove-all-collisions candidate-vics)
  (cleanse-from-null
    (map (lambda (x) (remove-collisions x candidate-vics)) candidate-vics)))
; suppose you are looking at (x a b s) in the input section:
; (remove-all-collisions '((-3 a x) (-4 a b) (0 s b)))
; --> ((-4 a b))

; given a relation as "first relation" in a triangle as well as
; the knowledge, create a list of triangulation hypotheses
; by finding a second relation and proposing a third relation -
; these still need to be consolidated as the third relation
; may actually already exist
(define (create-hypotheses firstrelation knowledge)
  (cleanse-from-null
    (ppmap (lambda (x)
      (cond
    
        ((and
          (not (null? x))
          (not (equal? (cdr firstrelation) (cdr x)))
          (not (equal? (reverse (cdr firstrelation)) (cdr x)))
          (equal? (cadr firstrelation) (cadr x)))
        (list
          (list 0 (caddr firstrelation) (caddr x))
          x
          firstrelation))
    
        ((and
          (not (null? x))
          (not (equal? (cdr firstrelation) (cdr x)))
          (not (equal? (reverse (cdr firstrelation)) (cdr x)))
          (equal? (caddr firstrelation) (cadr x)))
        (list
          (list 0 (cadr firstrelation) (caddr x))
          x
          firstrelation))
    
        ((and
          (not (null? x))
          (not (equal? (cdr firstrelation) (cdr x)))
          (not (equal? (reverse (cdr firstrelation)) (cdr x)))
          (equal? (cadr firstrelation) (caddr x)))
        (list
          (list 0 (caddr firstrelation) (cadr x))
          x
          firstrelation))
    
        ((and
          (not (null? x))
          (not (equal? (cdr firstrelation) (cdr x)))
          (not (equal? (reverse (cdr firstrelation)) (cdr x)))
          (equal? (caddr firstrelation) (caddr x)))
        (list
          (list 0 (cadr firstrelation) (cadr x))
          x
          firstrelation))
    
        (#t '())))
    
    knowledge)))
; (create-hypotheses '(10 b c) '((1 a b) (2 e f) (3 b c) (4 f g)
; (5 x y) (6 y z) (7 c a) (8 v x) (9 e g)))
; --> (((0 c a) (1 a b) (10 b c)) ((0 b a) (7 c a) (10 b c)))
; the above case is TWICE the SAME triangle, but that is OK,
; I will compute it TWICE
; as this saves me from having to "consolidate results" several times
; (which would mean serial action bottlenecks)

; multiple times the same triangulation can be undertaken
; instead of trying to "prevent" that, rather keep it just in mind
; when conducting triangulation
; to confirm the hypothesis in the first relation, try to match it to the
; second relation of another triangle
(define (consolidate-single triangle list-of-triangles)
  (cond

    ((null? list-of-triangles) triangle)

    ((and
      (equal? (caddr triangle) (caddar list-of-triangles))
      (or
        (equal? (cdar triangle) (cdadar list-of-triangles))
        (equal? (reverse (cdar triangle)) (cdadar list-of-triangles))))
    (cons (cadar list-of-triangles) (cdr triangle)))

    (#t (consolidate-single triangle (cdr list-of-triangles)))))
; (consolidate-single '((0 c a) (1 a b) (10 b c))
; '(((0 c a) (1 a b) (10 b c)) ((0 b a) (7 c a) (10 b c))))
; --> ((7 c a) (1 a b) (10 b c))

; just a little side-note:
; (map (lambda (y) (if (equal? y '((1 2) 1 2)) (cons y '(1 2)) '()))
; (map (lambda (x) (if (equal? x '(1 2))
; (cons x '(1 2)) '())) '((3 4) (1 2) (5 6) (7 8))))

(define (find-single-relation firstrelation knowledge)
  (let ((list-of-tri (create-hypotheses firstrelation knowledge)))
    (map (lambda (x) (consolidate-single x list-of-tri)) list-of-tri)))
; (find-single-relation '(3 b c) '((1 a b) (2 e f) (3 b c)
; (4 f g) (5 x y) (6 y z) (7 c a) (8 v x) (9 e g)))
; --> (((7 c a) (1 a b) (3 b c)) ((1 a b) (7 c a) (3 b c)))

; Now - given an input list "lis" and the "knowledge",
; establish which triangles shall participate in a reaction
; (define (find-all-relations lis knowledge)
;   (let ((paired (pair-up lis)))
;     (let ((vic-hypotheses
;             (remove-all-collisions
;               (find-potential-vics paired knowledge))))
;     (apply_append (append '() '()
;       (map (lambda (x) (find-single-relation x knowledge))
;                          vic-hypotheses))))))
; (find-all-relations '(a b c e f) '((-1 a b) (-2 e f) (-3 b c)
; (4 f g) (5 x y) (6 y z) (7 c a) (8 v x) (9 e g)))           
; -->
; (((7 c a) (-1 a b) (-3 b c))
;  ((-1 a b) (7 c a) (-3 b c))
;  ((9 e g) (4 f g) (-2 e f))
;  ((4 f g) (9 e g) (-2 e f)))

; (find-all-relations '(x y f g) '((-1 a b) (-2 e f) (-3 b c)
; (4 f g) (5 x y) (6 y z) (7 c a) (8 v x) (9 e g)))  
; --> () -- because "x y" and "f g" are positive (i.e. rather ana than vic)
; and "y f" is unknown
; NOTE THE POSSIBILITY THAT NOT EVEN ONE RELATION HAS BEEN FOUND!
; - Alright, see below:

(define (find-all-relations lis knowledge)
  (let ((paired (pair-up lis)))
    (let ((vic-hypotheses
            (remove-all-collisions
              (find-potential-vics paired knowledge))))
      (let ((tri-rel (apply_append (append '() '()
              (pmap (lambda (x) (find-single-relation x knowledge))
                   vic-hypotheses)))))
        (cond

          ((not (null? tri-rel))
          tri-rel)

          (#t
          (let ((force-pair
                (force-check-pair (car (reverse paired)) knowledge)))
            (cond ((not (null? force-pair))
                  (list (list force-pair force-pair force-pair)))

; else, create a hierarchisation by force from the last pair:
                  (#t
                  (let ((newpair (append '(0) (car (reverse paired)))))
                    (list (list newpair newpair newpair))))))))))))

; (find-all-relations '(aa bb) '((-1 a b) (-2 e f) (-3 b c) (4 f g) (5 x y)
; (6 y z) (7 c a) (10 b l) (12 l r) (-40 r g) (-10 b r) (8 a l) (8 v x)
; (9 e g) (1 x0 y0) (1 x1 y1) (1 x2 y2) (1 x3 y3) (1 x4 y4) (1 x5 y5)
; (1 x6 y6) (1 x7 y7) (1 x8 y8) (1 x9 y9)))
; -->
; (((0 aa bb) (0 aa bb) (0 aa bb)))


; auxiliary - see below
(define (triangulate one-triangle)
  (list

    (cons (+ (caar one-triangle)
             (* (sign (caadr one-triangle))
                (sign (caaddr one-triangle))
                (sqrt (* (abs (caadr one-triangle))
                         (abs (caaddr one-triangle))))))
      (cdar one-triangle))

    (cons (+ (caadr one-triangle)
             (* (sign (caar one-triangle))
                (sign (caaddr one-triangle))
                (sqrt (* (abs (caar one-triangle))
                         (abs (caaddr one-triangle))))))
      (cdadr one-triangle))

    (cons (+ (caaddr one-triangle)
             (* (sign (caar one-triangle))
                (sign (caadr one-triangle))
                (sqrt (* (abs (caar one-triangle))
                         (abs (caadr one-triangle))))))
      (cdaddr one-triangle))))
; positive triangle vic-ana-vic - strengthened:
; (triangulate '((-1 a b) (7 c a) (-3 b c)))              
; --> ((-5.58257569495584 a b) (8.732050807568877 c a) (-5.645751311064591 b c))
;
; negative triangle vic-vic-vic - weakened,
; relation flipped to ana-vic-vic which is positive:
; (triangulate '((-1 a b) (-7 c a) (-3 b c)))
; --> ((3.58257569495584 a b) (-5.267949192431123 c a) (-.3542486889354093 b c))

; THUNK - using *limtri*
(define (triangulation list-of-triangles)
  (ppmap (lambda (x) (triangulate x)) (takelast list-of-triangles *limtri*)))
; OPTIONALLY - THAT MAY ACTUALLY BE "TAKEFIRST" ABOVE.
; TAKELAST "DIVERSIFIES" TRIANGULATION WITH THE LATER TRIANGLES;
; TAKEFIRST "FOCUSES" TRIANGULATION TO THE EARLIER TRIANGLES.

; (triangulation (find-all-relations '(a b c e f) '((-1 a b) (-2 e f) (-3 b c)
; (4 f g) (5 x y) (6 y z) (7 c a) (8 v x) (9 e g))))
; -->
; (((8.732050807568877 c a) (-5.58257569495584 a b) (-5.645751311064591 b c))
;  ((-5.58257569495584 a b) (8.732050807568877 c a) (-5.645751311064591 b c))
;  ((6.17157287525381 e g) (-.24264068711928477 f g) (4 e f))
;  ((-.24264068711928477 f g) (6.17157287525381 e g) (4 e f)))

; implant the triangulation results into the knowledge
; CONTINUE HERE

; now, ultimately create a function "sum-up-triangle-relations"
; which will collect the results of triangulation in such a way
; that they can be implanted into the knowledge base

(define (sum-all-in-one one-relation list-of-relations)
  (cond

    ((null? list-of-relations)
    (list
; divide by two for the "doubled" hypotheses during matching,
; as I am creating two hypotheses:
      (cons
        (/ (car one-relation) 2)
        (cdr one-relation))))

    ((or
      (and
        (equal? (cadr one-relation) (cadar list-of-relations))
        (equal? (caddr one-relation) (caddar list-of-relations)))
      (and
        (equal? (cadr one-relation) (caddar list-of-relations))
        (equal? (caddr one-relation) (cadar list-of-relations))))
    (sum-all-in-one
      (cons
        (+ (car one-relation) (caar list-of-relations))
        (cdr one-relation))
      (cdr list-of-relations)))

    (#t
    (cons (car list-of-relations)
      (sum-all-in-one one-relation (cdr list-of-relations))))))
; (sum-all-in-one '(-1 a b) '((-2 e f) (3 a b) (-7 a b) (-3 b c) (4 f g)
; (5 x y) (6 y z) (7 c a) (8 v x) (9 e g)))
; -->
; ((-2 e f) (-3 b c) (4 f g) (5 x y) (6 y z) (7 c a) (8 v x) (9 e g)
; (-5/2 a b))


(define (sum-up-actual-results first-relation list-of-relations)
  (cond

    ((and
      (equal? (cadr first-relation) (cadar list-of-relations))
      (equal? (caddr first-relation) (caddar list-of-relations)))
    list-of-relations)

    (#t
      (sum-up-actual-results first-relation
        (sum-all-in-one (car list-of-relations) (cdr list-of-relations))))))

(define (sum-up-results list-of-relations)
  (cond ((null? list-of-relations) '())
        (#t (sum-up-actual-results
              (car list-of-relations)
              (sum-all-in-one
                (car list-of-relations)
                (cdr list-of-relations))))))
; (sum-up-results '((-2 e f) (3 b a) (-7 a b) (-3 b c) (4 f g) (5 x y)
; (6 y z) (7 c a) (8 v x) (-27 c a) (9 e g)))
; -->
; ((-1 e f)
;  (-2 b a)
;  (-3/2 b c)
;  (2 f g)
;  (5/2 x y)
;  (3 y z)
;  (-10 c a)
;  (4 v x)
;  (9/2 e g))

(define (sum-up-triangle-relations list-of-triangles)
  (sum-up-results (apply_append (append '() '() list-of-triangles))))

; (sum-up-triangle-relations (triangulation (find-all-relations '(a b c e f)
; '((-1 a b) (-2 e f) (-3 b c) (4 f g) (5 x y)
; (6 y z) (7 c a) (8 v x) (9 e g)))))
; -->
; ((8.732050807568877 c a)
; (-5.58257569495584 a b)
; (-5.645751311064591 b c)
; (6.17157287525381 e g)
; (-.24264068711928477 f g)
; (4 e f))

; next - actually get a function to adjust the knowledge
; based on the above relation changes that were found

; you will need a forgetting mechanism for the incorporation of new relations

; auxiliary
(define (adjust-one-knowledge-relation relation knowledge)
  (cond

; this is a signal for the forget-and-replace-mechanism:
    ((null? knowledge) '())

    ((or
      (and
        (equal? (cadr relation) (cadar knowledge))
        (equal? (caddr relation) (caddar knowledge)))
      (and
        (equal? (cadr relation) (caddar knowledge))
        (equal? (caddr relation) (cadar knowledge))))
    (cons
      (cons

        ; (+ (car relation) (caar knowledge))
        ; instead of the above - try below s-expression for smaller numbers:
        (* (sign (car relation)) (sign (caar knowledge))
           (sqrt (* (abs (car relation)) (abs (caar knowledge)))))

        (cdr relation))
      (cdr knowledge)))

    (#t
; make sure the above signal is propagated back through the conses:
    (let ((check-for-null
            (adjust-one-knowledge-relation relation (cdr knowledge))))
        (cond
          ((null? check-for-null) '())
          (#t (cons (car knowledge) check-for-null)))))))
; (adjust-one-knowledge-relation '(1300 y z)
; '((-1 a b) (-2 e f) (-3 b c) (4 f g) (5 x y)
; (6 y z) (7 c a) (8 v x) (9 e g)))                                  
; -->
; ((-1 a b) (-2 e f) (-3 b c) (4 f g) (5 x y)
; (1306 y z) (7 c a) (8 v x) (9 e g))
;
; (adjust-one-knowledge-relation '(123 xxx yyy)
; '((-10 a b) (-20 e f) (-3 b c) (4 f g) (5 x y)
; (6 y z) (7 c a) (8 v x) (9 e g)))
; -->
; ()

; if a relation is new, implant it instead of the weakest relation
; instead of looking really for the "really" weakest, I could say
; "weakest or below threshold" as a crummy relation is anyway
; going to be flipped and I could save some time

; ; while that will work, it is simply too much fuss for a system
; ; that actually tries to understand the world in parallel
; ; and thus ANYWAY makes pure ASSUMPTIONS about what matters
; ; auxiliary for below
; (define (proto-forget-and-create relation knowledge candidate
;                                  seen remaining
;                                  trace-seen trace-remaining)
;   (cond
; 
;     ((null? knowledge)
;     (append (reverse seen) (list relation) remaining))
; 
;     ((> (abs (car candidate)) (abs (caar knowledge)))
;     (proto-forget-and-create
;       relation
;       (cdr knowledge)
;       (car knowledge)
;       trace-seen
;       (cdr knowledge)
;       trace-seen
;       (cdr knowledge)))
; 
;     (#t
;     (proto-forget-and-create
;       relation
;       (cdr knowledge)
;       candidate
;       seen
;       remaining
;       (cons (car knowledge) trace-seen)
;       (cdr knowledge)))))
; 
; (define (forget-and-create relation knowledge)
;   (proto-forget-and-create
;     relation
;     (cdr knowledge)
;     (car knowledge)
;     '() (cdr knowledge)
;     (list (car knowledge)) (cdr knowledge)))
; ; (forget-and-create '(123 xxx yyy) '((-10 a b) (-20 e f) (-3 b c)
; ; (4 f g) (5 x y) (6 y z) (7 c a) (8 v x) (9 e g)))  
; ; -->
; ; ((-10 a b) (-20 e f) (123 xxx yyy) (4 f g)
; ; (5 x y) (6 y z) (7 c a) (8 v x) (9 e g))

; let's do something shameless and fast -
; simply shift off the oldest element,
; no matter how "useful" it is in practice;
; if it is really that useful, it will be
; re-learned and re-created anyway
(define (forget-and-create relation knowledge)
  (cons relation (butlast knowledge)))
; (forget-and-create '(123 xxx yyy) '((-10 a b) (-20 e f) (-3 b c)
; (4 f g) (5 x y) (6 y z) (7 c a) (8 v x) (9 e g)))  
; -->
; ((123 xxx yyy) (-10 a b) (-20 e f) (-3 b c)
; (4 f g) (5 x y) (6 y z) (7 c a) (8 v x))

(define (adjust-or-create relation knowledge)
  (let ((try-to-adjust (adjust-one-knowledge-relation relation knowledge)))
    (cond
      ((null? try-to-adjust) (forget-and-create relation knowledge))
      (#t try-to-adjust))))
; new relation - created:
; (adjust-or-create '(123 xxx yyy) '((-10 a b) (-20 e f) (-3 b c) (4 f g)
; (5 x y) (6 y z) (7 c a) (8 v x) (9 e g)))              
; -->
; ((123 xxx yyy) (-10 a b) (-20 e f) (-3 b c) (4 f g)
; (5 x y) (6 y z) (7 c a) (8 v x))
;
; known relation - adjusted:
; (adjust-or-create '(123 y z) '((-10 a b) (-20 e f) (-3 b c) (4 f g)
; (5 x y) (6 y z) (7 c a) (8 v x) (9 e g)))     
; -->
; ((-10 a b) (-20 e f) (-3 b c) (4 f g) (5 x y)
; (129 y z) (7 c a) (8 v x) (9 e g))

; this function, finally, is to be used after triangulation
; has been performed - but it can be "multi-level triangulation",
; there is no necessity to use it "immediately" after one level
(define (adjust-knowledge list-of-relations knowledge)
  (cond ((null? list-of-relations) knowledge)
        (#t (adjust-knowledge (cdr list-of-relations)
              (adjust-or-create (car list-of-relations) knowledge)))))
; here a one-level experiment to demonstrate its general usage:
; (adjust-knowledge (sum-up-triangle-relations
; (triangulation (find-all-relations '(a b c e f)
; '((-1 a b) (-2 e f) (-3 b c) (4 f g) (5 x y) (6 y z) (7 c a) (8 v x)
; (9 e g))))) '((-1 a b) (-2 e f) (-3 b c) (4 f g) (5 x y) (6 y z)
; (7 c a) (8 v x) (9 e g)))
; -->
; ((-6.58257569495584 a b)
; (2 e f)
; (-8.64575131106459 b c)
; (3.7573593128807152 f g)
; (5 x y)
; (6 y z)
; (15.732050807568877 c a)
; (8 v x)
; (15.17157287525381 e g))

; sledge and snowflake reconsideration mechanisms COMBINED:
; you can now take each of the found relations and try to
; triangulate further along them - which is the "snowflake",
; but as you include the original relations, too, you have
; also the "sledge"
; by appending the triangles, you easily extract their relations
(define (extend-triangulation count knowledge list-of-triangles)
  (cond

; do not even adjust the knowledge if nothing is to be done:
    ((zero? count)
    knowledge)

; merely adjust the knowledge after, apparently, at least one triangulation:
    ((equal? 1 count)
    (adjust-knowledge
      (sum-up-triangle-relations list-of-triangles)
      knowledge))

; on with the show - triangulate and pursue the relations:
    (#t
    (let ((relations
            (apply_append (append '() '() list-of-triangles))))
      (extend-triangulation
        (- count 1)
        (adjust-knowledge
          (sum-up-triangle-relations list-of-triangles)
          knowledge)
        (triangulation
          (apply_append (append '() '()
            (pmap (lambda (x)
              (find-single-relation x knowledge))
              relations)))))))))
; (extend-triangulation 3 '((-1 a b) (-2 e f) (-3 b c) (4 f g) (5 x y)
; (6 y z) (7 c a) (8 v x) (9 e g))
; (triangulation (find-all-relations '(a b c e f)
; '((-1 a b) (-2 e f) (-3 b c) (4 f g) (5 x y)
; (6 y z) (7 c a) (8 v x) (9 e g)))))
; -->
; ((-691.8236366529367 a b)
;  (290.96561463223213 e f)
;  (-710.70159151838 b c)
;  (291.45630036777686 f g)
;  (5 x y)
;  (6 y z)
;  (853.5730037303896 c a)
;  (8 v x)
;  (601.6966512330873 e g))
; or
; ((-12.624115294434555 a b)
; (23.806544563944957 e f)
; (-17.505819186975824 b c)
; (16.50867738632829 f g)
; (5 x y)
; (6 y z)
; (71.04890521831679 c a)
; (8 v x)
; (74.18538581459472 e g))
; - if the sqrt-adjustment is chosen
;
; (extend-triangulation 2 '((-1 a b) (-2 e f) (-3 b c) (4 f g) (5 x y)
; (6 y z) (7 c a) (10 b l) (12 l r) (-40 r g) (-10 b r) (8 a l) (8 v x)
; (9 e g) (1 x0 y0) (1 x1 y1) (1 x2 y2) (1 x3 y3) (1 x4 y4) (1 x5 y5)
; (1 x6 y6) (1 x7 y7) (1 x8 y8) (1 x9 y9))
; (triangulation (find-all-relations '(a b c e f)
; '((-1 a b) (-2 e f) (-3 b c) (4 f g) (5 x y) (6 y z) (7 c a) (10 b l)
; (12 l r) (-40 r g) (-10 b r) (8 a l) (8 v x) (9 e g) (1 x0 y0) (1 x1 y1)
; (1 x2 y2) (1 x3 y3) (1 x4 y4) (1 x5 y5) (1 x6 y6) (1 x7 y7) (1 x8 y8)
; (1 x9 y9)))))
; -->
; ((3.1153856077171875 f r)
;  (-15.711871785696076 e r)
;  (10 b g)
;  (-7.400828044922853 c g)
;  (12.148790924169553 a r)
;  (9.191093814044986 c r)
;  (6.410377412118246 c l)
;  (-10.838606979927595 a b)
;  (-7.3272040640506315 e f)
;  (-22.497323015593487 b c)
;  (-3.4037988265302452 f g)
;  (5 x y)
;  (6 y z)
;  (26.810819948947685 c a)
;  (18.980906574286838 b l)
;  (13.856406460551018 l r)
;  (69.2820323027551 r g)
;  (-20.10159286742583 b r)
;  (11.20800362300473 a l)
;  (8 v x)
;  (20.8358721997742 e g)
;  (1 x0 y0)
;  (1 x1 y1)
;  (1 x2 y2))

; THUNK
; this function gives "energy" to the hypotheses so that there is something
; to "consider" during triangulation
; (define (energise vic-relations)
;   (map
;     (lambda (x) (cons (- (car x) *energy-token*) (cdr x))) vic-relations))

(define (energise list-of-triangles)
  (map (lambda (triangle)
        (list
          (car triangle)
          (cadr triangle)
          (cons (- (caaddr triangle) *energy-token*) (cdaddr triangle))))
    list-of-triangles))
; (energise (find-all-relations '(a b c e f) '((-1 a b) (-2 e f) (-3 b c)
; (4 f g) (5 x y) (6 y z) (7 c a) (8 v x) (9 e g))))
; -->
; (((7 c a) (-1 a b) (-7 b c))
;  ((-1 a b) (7 c a) (-7 b c))
;  ((9 e g) (4 f g) (-6 e f))
;  ((4 f g) (9 e g) (-6 e f)))
;
; without that, the triangles are just:
;  (find-all-relations '(a b c e f) '((-1 a b) (-2 e f) (-3 b c)
; (4 f g) (5 x y) (6 y z) (7 c a) (8 v x) (9 e g)))           
; -->
; (((7 c a) (-1 a b) (-3 b c))
;  ((-1 a b) (7 c a) (-3 b c))
;  ((9 e g) (4 f g) (-2 e f))
;  ((4 f g) (9 e g) (-2 e f)))

; make sure the values of vic and ana are not blown out of proportion
(define (cutval knowledge)
  (ppmap (lambda (x)
          (cond
            ((>= *maxabs* (abs (car x))) x)
            (#t (cons (* (sign (car x)) *maxabs*) (cdr x)))))
    knowledge))

; THUNK
; IMPORTANT - tie up everything together in order
; to adjust knowledge due to input - this is a THUNK
; using the *triangulation-count* global variable;
; consider here the fact that perhaps you find NO
; known relation or only a known ANA-relation, but
; NO VIC relation

; Now incorporates the "energise" function which gives
; hypotheses "energy" for their consideration

(define (triangulate-and-adjust-knowledge input-list knowledge)
  (cutval
    (extend-triangulation
      *triangulation-count*
      knowledge

; IF YOU WISH TO IMPLEMENT A SORT OF SHORT-TERM MEMORY:
      (triangulation (energise (find-all-relations input-list
        (takefirst knowledge *depth-of-short-term-memory*)))))))
; IF YOU RATHER WANT TO USE ALL KNOWLEDGE FOR CONSIDERATION:
;     (triangulation (energise (find-all-relations input-list knowledge))))))

; (triangulate-and-adjust-knowledge '(a b c e f)
; '((-1 a b) (-2 e f) (-3 b c) (4 f g) (5 x y) (6 y z) (7 c a) (10 b l)
; (12 l r) (-40 r g) (-10 b r) (8 a l) (8 v x) (9 e g) (1 x0 y0)
; (1 x1 y1) (1 x2 y2) (1 x3 y3) (1 x4 y4) (1 x5 y5) (1 x6 y6) (1 x7 y7)
; (1 x8 y8) (1 x9 y9)))
; -->
; ((11.573191830000114 f r)
;  (-12.807842151484603 e r)
;  (10 b g)
;  (-9.146912192286944 c g)
;  (14.351838638835176 a r)
;  (13.304001841018092 c r)
;  (10.43760766282316 c l)
;  (-14.173547897800374 a b)
;  (0 e f)
;  (-31.187123449140387 b c)
;  (5.497660998324643 f g)
;  (5 x y)
;  (6 y z)
;  (28.380385720186787 c a)
;  (18.66574894721265 b l)
;  (13.856406460551018 l r)
;  (69.2820323027551 r g)
;  (-20.26910433718182 b r)
;  (9.50634429097943 a l)
;  (8 v x)
;  (17.0450001044491 e g)
;  (1 x0 y0)
;  (1 x1 y1)
;  (1 x2 y2))

; --------------------------------------------------

; ; No - do not do that - instead, say, "hierarchisation"
; ; contains co-ordinates and "triangulation" does NOT.

; (define (generate-coordinates vic-list pair-list knowledge)
;   (cond
; 
;     ((null? vic-list)
;     '())
; 
;     (#t
; ; the '() is just to ensure append will always work:
;     (append '()
;       (cond
; 
; ; the direction is correct:
;         ((mmb (cdar vic-list) pair-list)
;         (list
;                   (list *dim-minus* (cadar vic-list))
;                   (list *dim-plus* (caddar vic-list))))
; 
; ; else the direction is reversed - the IS a match definitely,
; ; as vic-list is actually generated out of pair-list
; ;       ((mmb (reverse (cdar vic-list)) pair-list) ; ) instead just #t:
;         (#t
;         (list
;                   (list *dim-plus* (cadar vic-list))
;                   (list *dim-minus* (caddar vic-list)))))
; 
;       (generate-coordinates (cdr vic-list) pair-list knowledge)))))
; ; (generate-coordinates (remove-all-collisions
; ; (find-potential-vics (pair-up '(a b c d e f))
; ; '((-1 a b) (-2 e f) (-3 b c) (4 f g) (5 x y) (6 y z) (7 c a) (10 b l)
; ; (12 l r) (-40 r g) (-10 b r) (8 a l) (8 v x) (9 e g) (1 x0 y0) (1 x1 y1)
; ; (1 x2 y2) (1 x3 y3) (1 x4 y4) (1 x5 y5) (1 x6 y6) (1 x7 y7) (1 x8 y8)
; ; (1 x9 y9))))
; ; (pair-up '(a b c d e f)) '((-1 a b) (-2 e f) (-3 b c) (4 f g) (5 x y)
; ; (6 y z) (7 c a) (10 b l) (12 l r) (-40 r g) (-10 b r) (8 a l) (8 v x)
; ; (9 e g) (1 x0 y0) (1 x1 y1) (1 x2 y2) (1 x3 y3) (1 x4 y4) (1 x5 y5)
; ; (1 x6 y6) (1 x7 y7) (1 x8 y8) (1 x9 y9)))
; ; -->
; ; ((*dim-minus* b) (*dim-plus* c) (*dim-minus* e) (*dim-plus* f))

; --------------------------------------------------

; this function is somewhat fussy and generates, repeatedly applied,
; ((a ((b c) (e f)))) from the below example - the issue with this is
; that a plan relates to the "last part of the present", and this way,
; this is a (). So I need to take the "car" of that in order to get
; a plan.
(define (generate-hierarchisation cdr-vic-list input-list)
  (cond

; the first two cases should not happen at all:
; WELL THE FOLLOWING ONE DOES HAPPEN AND IS THE
; TERMINATION CONDITION - IT IS FINALLY THE "LISTING" REASON
    ((null? input-list)
    '())

    ((null? (cdr input-list))
    input-list)

    (#t
    (let ((current-pair (list (car input-list) (cadr input-list)))
          (list-after-pair (takeafter input-list 2)))

      (cond

        ; THESE ARE THE CULPRINTS FOR WEIRD HIERARCHISATIONS:
        ; THE CONS TO, FINALLY, () IS THE SAME AS AN EXTRA "LIST"

        ((or (mmb current-pair cdr-vic-list)
             (mmb (reverse current-pair) cdr-vic-list))
        (cons current-pair
          (generate-hierarchisation cdr-vic-list list-after-pair)))

        (#t
        (cons (car input-list)
          (generate-hierarchisation cdr-vic-list (cdr input-list)))))))))
; (generate-hierarchisation
; (map cdr (remove-all-collisions (find-potential-vics
; (pair-up '(a b c d e f))
; '((-1 a b) (-2 e f) (-3 c b) (4 f g) (5 x y) (6 y z) (7 c a)
; (10 b l) (12 l r) (-40 r g) (-10 b r) (8 a l) (8 v x) (9 e g)
; (1 x0 y0) (1 x1 y1) (1 x2 y2) (1 x3 y3) (1 x4 y4) (1 x5 y5)
; (1 x6 y6) (1 x7 y7) (1 x8 y8) (1 x9 y9))))) '(a b c d e f))
; -->
; (a (b c) d (e f))

; --------------------------------------------------

; auxiliary
(define (ecar x) (cond ((null? x) '()) ((list? x) (car x)) (#t x)))

; a bit unusual - I really hierarchise up to the top
; list-of-input-and-knowledge simply has input and
; knowledge "packaged" into one list in order to
; perform recursion
(define (cnt-hierarchisation maxcounter list-of-input-and-knowledge)
  (cond

; if there is no input to hierarchise, output input and knowledge:
    ((or (zero? maxcounter) (> 2 (length (car list-of-input-and-knowledge))))
; originally, it was just "list-of-input-and-knowledge", but
; now I am re-consing it in order to remove the final "listing"
; of the present - in order to be able to find a plan: 
    (cons (ecar (car list-of-input-and-knowledge))
          (cdr list-of-input-and-knowledge)))

    (#t
    (let ((adjusted-knowledge
            (triangulate-and-adjust-knowledge
              (car list-of-input-and-knowledge)
              (cadr list-of-input-and-knowledge))))
      (cnt-hierarchisation (- maxcounter 1) (list
        (generate-hierarchisation
          (map cdr
            (remove-all-collisions (find-potential-vics
              (pair-up (car list-of-input-and-knowledge))
              adjusted-knowledge)))
          (car list-of-input-and-knowledge))
        adjusted-knowledge))))))

; INSERTING A COUNTER HERE TO LIMIT CNT-HIERARCHISATION
; FROM LOOPING INFINITELY. THIS IS MORE OF A DEBUGGING HACK.
(define (hierarchisation list-of-input-and-knowledge)
  (cnt-hierarchisation (length (car list-of-input-and-knowledge))
                       list-of-input-and-knowledge))

; (hierarchisation '((a b c e f) ((-1 a b) (-2 e f) (-3 b c) (4 f g)
; (5 x y) (6 y z) (7 c a) (10 b l) (12 l r) (-40 r g) (-10 b r) (8 a l)
; (8 v x) (9 e g) (1 x0 y0) (1 x1 y1) (1 x2 y2) (1 x3 y3) (1 x4 y4)
; (1 x5 y5) (1 x6 y6) (1 x7 y7) (1 x8 y8) (1 x9 y9))))
; -->
; in the first line is the hierarchised present in the car,
; and in the cadr is the knowledge:
; (((a ((b c) (e f))))
;  ((64.16362010311445 c l)
;   (32.781853143388666 b l)
;   (-133.67375029990552 b c)
;   (1.7578771433106777 a g)
;   (250 c a)
;   (-6.778939516432097 a e)
;   (6.443923138904731 a f)
;   (-89.3321423620627 b r)
;   (69.2820323027551 r g)
;   (-2.1810455315814856 ((b c) (e f)) g)
;   (64.93017135791881 l r)
;   (4.925494870805099 ((b c) (e f)) e)
;   (-4.682076052102429 ((b c) (e f)) f)
;   (22.959190813473015 a l)
;   (9.477750501941125 ((b c) (e f)) l)
;   (18.556784552786002 ((b c) (e f)) c)
;   (12.845534640787529 ((b c) (e f)) b)
;   (20.95748140734316 ((b c) (e f)) a)
;   (14.877230002461104 ((b c) (e f)) r)
;   (-2 (b c) (e f))
;   (11.573191830000114 f r)
;   (12.807842151484603 e r)
;   (10 b g)
;   (9.146912192286944 c g)))

; DO "PLANNING"

; A PLAN IS NO LONGER "SPECIFICALLY THE OTHER ATOM"
; AS YOU HAVE NO IDEA WHETHER THAT ATOM IS REALLY
; "SECOND" - IT COULD BE "FIRST"
; therefore, you first find possible plans at all;
; and then you try to see whether they are used
; anywhere as "second" after the present WITHIN
; a cadr or a caddr - as these are hierarchisations,
; these IMPLY co-ordinates
; and then the best one of them is the plan
; there might be no plan whatsoever
; remember, you are only looking for vic-relations,
; i.e. zero or negative

(define (find-potential-plans present knowledge)
  (cleanse-from-null
    (ppmap (lambda (x)
            (cond
; YOU CAN TURN OFF (CAR X) BELOW IN ORDER TO ALLOW EASIER PLAN SEARCHES
; IF THERE IS A NEGATIVE PLAN IT WILL BE SELECTED LATER AS BEST RESULT
              ((and (>= 0 0) ; (car x))
                    (equal? present (cadr x))) (list (caddr x) (car x)))
              ((and (>= 0 0) ; (car x))
                    (equal? present (caddr x))) (list (cadr x) (car x)))
              (#t '())))
      knowledge)))
; (find-potential-plans '(a b)
; '((3 x y) (-7 (c d) (a b)) (18 ((a b) (c d)) q)
; (-1 (a b) (u v)) (-10 (a b) (l m)) (-2 r ((a b) (l m)))))
; -->
; (((c d) -7) ((u v) -1) ((l m) -10))
; 
; (find-potential-plans 'too-high-hierarchy (cadr (hierarchisation
; '((a b c e f) ((-1 a b) (-2 e f) (-3 b c) (4 f g) (5 x y) (6 y z)
; (7 c a) (10 b l) (12 l r) (-40 r g) (-10 b r) (8 a l) (8 v x) (9 e g)
; (1 x0 y0) (1 x1 y1) (1 x2 y2) (1 x3 y3) (1 x4 y4) (1 x5 y5)
; (1 x6 y6) (1 x7 y7) (1 x8 y8) (1 x9 y9))))))
; -->
; ()
; - in other words, if NO plan can be found, then you will have to decompose
; the present and try to continue its "right half", and so forth - UNTIL
; the right half is "not a list", in which case you output '() AS THE RESULT.
;
; (find-potential-plans '((b c) (e f)) (cadr (hierarchisation '((a b c e f)
; ((-1 a b) (-2 e f) (-3 b c) (4 f g) (5 x y) (6 y z) (7 c a) (10 b l)
; (12 l r) (-40 r g) (-10 b r) (8 a l) (8 v x) (9 e g) (1 x0 y0) (1 x1 y1)
; (1 x2 y2) (1 x3 y3) (1 x4 y4) (1 x5 y5) (1 x6 y6) (1 x7 y7) (1 x8 y8)
; (1 x9 y9))))))
; -->
; ((g -2.1810455315814856) (f -4.682076052102429))

; auxiliary
; now try to show that (list present plan-candidate) is known anywhere:
(define (prove-potential-plan present-with-plan knowledge)
  (cond
    ((null? knowledge) '())
    ((or (equal? present-with-plan (cadar knowledge))
         (equal? present-with-plan (caddar knowledge))) #t)
    (#t (prove-potential-plan present-with-plan (cdr knowledge)))))

; auxiliary: shows #t or () depending on whether a plan has been "confirmed"
(define (check-possible-plans present potentials knowledge)
    (cond ((null? potentials) '())
          (#t (pmap (lambda (x)
                (prove-potential-plan (list present (car x)) knowledge))
                potentials))))

; auxiliary: in order to compare
; a "confirmations' list" for plans with the plans:
(define (kill-nulls lis1 lis2)
  (cond
    ((or (null? lis1) (null? lis2)) '())
    ((null? (car lis1)) (kill-nulls (cdr lis1) (cdr lis2)))
    (#t (cons (car lis2) (kill-nulls (cdr lis1) (cdr lis2))))))
; (kill-nulls '(#t #t () () #t) '(a b c d e)) --> (a b e)

; actually find a plan for a given version of the present - if possible:
(define (establish-plans present knowledge)
  (let ((potentials (find-potential-plans present knowledge)))
    (kill-nulls
      (check-possible-plans present potentials knowledge)
      potentials)))
; (establish-plans '(a b) '((3 x y) (-7 (c d) (a b)) (18 ((a b) (c d)) q)
; (-1 (a b) (u v)) (-10 (a b) ((l m) (n o))) (-2 r ((a b) ((l m) (n o))))))
; --> (((c d) -7) (((l m) (n o)) -10))
; (establish-plans '(aa bb) '((3 x y) (-7 (c d) (a b)) (18 ((a b) (c d)) q)
; (-1 (a b) (u v)) (-10 (a b) ((l m) (n o))) (-2 r ((a b) ((l m) (n o))))))
; --> ()

; now, focus on the "most recent" present - until you
; get a plan or until it is sure that there is no plan
; make it a "prototype"
; the real function will "go back into the present" and
; try to find another answer if continuing the right-most branch
; delivers no result
(define (decompose-and-plan proto-present knowledge)
  (let ((present
          (cond
            ; ((not (list? proto-present)) (list proto-present '()))
            (#t proto-present))))
  (let ((planlist (establish-plans present knowledge)))
    (cond

      ((not (null? planlist)) planlist)

      ((and (null? planlist) (and (list? present) (not (null? present))))
; instead of "just trying" with the cadr, I am now "proposing" a plan:
; in essence, this is a form of back-tracking; if it does not work
; with the cadr, at all levels, then eventually, the plan-formation
; will be tried with the car. - This sub-division is going to happen
; all all lower levels, too.
      (let ((propose-plan
              (decompose-and-plan (cadr present) knowledge)))
; OK, RETURN IT TO NOT SAYING ANYTHING IF NO ANSWER IS KNOWN: ((
;       (cond ((not (null? propose-plan)) propose-plan)
;             (#t (decompose-and-plan (car present) knowledge)))))
        (cond (#t propose-plan))))

      (#t '())))))
; (decompose-and-plan '(a b) '((3 x y) (-7 (c d) (a b)) (18 ((a b) (c d)) q)
; (-1 (a b) (u v)) (-10 (a b) ((l m) (n o))) (-2 r ((a b) ((l m) (n o))))))
; --> (((c d) -7) (((l m) (n o)) -10))
; (decompose-and-plan '(aa bb) '((3 x y) (-7 (c d) (a b)) (18 ((a b) (c d)) q)
; (-1 (a b) (u v)) (-10 (a b) ((l m) (n o))) (-2 r ((a b) ((l m) (n o))))))
; --> ()
; (decompose-and-plan '((i j) (a b))
; '((3 x y) (-7 (c d) (a b)) (18 ((a b) (c d)) q)
; (-1 (a b) (u v)) (-10 (a b) ((l m) (n o))) (-2 r ((a b) ((l m) (n o))))))
; --> (((c d) -7) (((l m) (n o)) -10))

(define (select-best-plan candidate list-of-plans)
  (cond
    ((null? list-of-plans)
    (car candidate))

    ((> (cadr candidate) (cadar list-of-plans))
    (select-best-plan (car list-of-plans) (cdr list-of-plans)))

    (#t
    (select-best-plan candidate (cdr list-of-plans)))))

; finally, this is the function which generates a plan from a given present:
(define (do-planning present knowledge)
  (let ((planlist (decompose-and-plan present knowledge)))
    (cond ((null? planlist) '())
          (#t (flatten-list
                (select-best-plan (car planlist) (cdr planlist)))))))
; (do-planning '((i j) (a b)) '((3 x y) (-7 (c d) (a b))
; (18 ((a b) (c d)) q) (-1 (a b) (u v))
; (-10 (a b) ((l m) (n o))) (-2 r ((a b) ((l m) (n o))))))
; --> (l m n o)

; EARLY EXPERIMENT:
; (define kn '((0 () ()) (0 () ()) (0 () ()) (0 () ()) (0 () ())
; (0 () ()) (0 () ()) (0 () ()) (0 () ()) (0 () ())
; (0 () ()) (0 () ()) (0 () ()) (0 () ()) (0 () ())
; (0 () ()) (0 () ()) (0 () ()) (0 () ()) (0 () ())
; (0 () ()) (0 () ()) (0 () ()) (0 () ()) (0 () ())
; (0 () ()) (0 () ()) (0 () ()) (0 () ()) (0 () ())
; (0 () ()) (0 () ()) (0 () ()) (0 () ()) (0 () ())
; (0 () ()) (0 () ()) (0 () ()) (0 () ()) (0 () ())
; (0 () ()) (0 () ()) (0 () ()) (0 () ()) (0 () ())
; (0 () ()) (0 () ()) (0 () ()) (0 () ()) (0 () ()))
;
; (hierarchisation (list '(x y x y x y) kn))
; -->
; (((((x y) (x y)) (x y)))
;  ((.45971733191108055 ((x y) (x y)) (x y))
;   (-1.6817928305074292 (x y) (x y))
;   (-2 x y)
;   (0 () ())
;   (0 () ())
;   ...
;   (0 () ())
;   (0 () ())
;   (0 () ())))
;
; - ultimately, this (i) "boots up" and (ii) "guesses correctly"
; that (x y) is to be continued with (x y)
; END OF EARLY EXPERIMENT

; OPTIONAL: MULTIPLE KNOWLEDGE BASES ("INSECTS") + PARSING OF TEXT INPUT

(define INSTINCTS '((I YOU)
(i you)
(ME YOU)
(me you)
(YOU ME)
(you me)
(MYSELF YOURSELF)
(myself yourself)
(YOURSELF MYSELF)
(yourself myself)
(MY YOUR)
(my your)
(YOUR MY)
(your my)
(MINE YOURS)
(mine yours)
(YOURS MINE)
(yours mine)
(AM ARE)
(am are)
(ARE |AM/ARE|)
(are |am/are|)
(WAS |WAS/WERE|)
(was |was/were|)
(WERE |WAS/WERE|)
(were |was/were|)
(|I'M| |YOU'RE|)
(|i'm| |you're|)
(|YOU'RE| |I'M|)
(|you're| |i'm|)))

(define (proto-instinct-element someelement listofinstincts)
  (if (null? listofinstincts) someelement
    (if (equal? (caar listofinstincts) someelement) (cadar listofinstincts)
      (proto-instinct-element someelement (cdr listofinstincts)))))

(define (proto-instinct-list somelist listofinstincts resultlist)
  (if (null? somelist) (reverse resultlist)
    (proto-instinct-list (cdr somelist) listofinstincts
      (cons (proto-instinct-element (car somelist) listofinstincts)
             resultlist))))

(define (instinct-list somelist)
  (proto-instinct-list somelist INSTINCTS '()))

(define *human* '())
(define *machine* '())
(define *history* '())
(define *hierarchy* '())
(define *present* '())
(define *history-bkp* '())

(define *knowledge* (with-input-from-file "TRIDATA.TXT" read))

(define (eexxiitt)
  (begin (newline)
  (with-output-to-file "TRIDATAN.TXT" (lambda () (display *knowledge*)))
  (exit)))

; the pseudo-run will adjust the knowledge and propose a machine reply,
; but it will NOT "read in" information, it will NOT output data,
; and it will NOT adjsut history - thi is left to the "outer" process
; so that
; sliding (reconsideration)
; and snowflake (reconsideration with the original history)
; can be implemented to "extend" thinking.
(define (pseudo-run)
  (begin
    (set! *hierarchy* (hierarchisation (list *history* *knowledge*)))
    (set! *present* (car *hierarchy*))
    (set! *knowledge* (cadr *hierarchy*))
    (set! *machine* (do-planning *present* *knowledge*))))

(define (RUN)
  (begin

  (display '(HUMAN------))
  (set! *human* (read))
  (cond ((null? *human*) (eexxiitt))
        ((not (list? *human*))
          (begin
          (newline)
          (newline)
          (display '(NON LIST INPUT ENTERING REPL))
          (newline)
          (newline)))
        ((or (equal? *human* '(DUMP)) (equal? *human* '(dump)))
          (begin
          (newline)
          (newline)
          (display '(BEGIN DUMP))
          (newline)
          (newline)
          (display '(HISTORY))
          (newline)
          (display *history*)
          (newline)
          (newline)
          (display '(HIERARCHISATION OF THE PREVIOUS PRESENT))
          (newline)
          (display *present*)
          (newline)
          (newline)
          (display '(KNOWLEDGE FIRST 50 RELATIONS))
          (newline)
          (display (takefirst *knowledge* 50))
          (newline)
          (newline)
          (display '(END DUMP CONTINUE CONVERSATION))
          (newline)
          (newline)
          (newline)
          (RUN)))
        (#t
  (begin
  (set! *history* (takelast (append *history* *human*) *history-length*))

; the below is a "reconsideration" section which
; generally should make the system "smarter"
; to accelerate (but make less intelligent), you may comment out from here...

  (set! *history-bkp* *history*)

  ; example of snowflake
  (set! *history* *history-bkp*)
  (pseudo-run)

  ; example of sliding
;   (set! *machine* (instinct-list *machine*))
  (set! *history* (takelast (append *history* *machine*) *history-length*))
  (pseudo-run)

  ; example of snowflake
  (set! *history* *history-bkp*)
  (pseudo-run)

  ; example of sliding
;   (set! *machine* (instinct-list *machine*))
  (set! *history* (takelast (append *history* *machine*) *history-length*))
  (pseudo-run)

  (set! *history* *history-bkp*)

; ... till here

  (pseudo-run)
  (set! *machine* (instinct-list *machine*))
  (set! *history* (takelast (append *history* *machine*) *history-length*))
  (display '(MACHINE----))
  (display *machine*)
  (newline)
  (RUN))))))

(define (MAIN)
  (newline)
  (newline)
  (display '(LOGICAL TRIANGULATION NUMERIC VERSION))
  (newline)
  (display '(ENTER (LIST OF UP TO 30 WORDS) TO TALK AND () TO QUIT))
  (newline)
  (display '(ENTER A SYMBOL TO EXIT TO REPL AND (MAIN) OR (RUN) TO RESTART))
  (newline)
  (display '(MOVE TRIDATAN.TXT TO TRIDATA.TXT AFTER CHAT TO LEARN))
  (newline)
  (display '(OTHERWISE DELETE TRIDATAN.TXT AFTER CHATTING))
  (newline)
  (display '(FOR THE HISTORY THE PRESENT AND A PART OF THE KNOWLEDGE ENTER))
  (newline)
  (display '(DUMP))
  (newline)
  (display '(AT ANY TIME AS YOUR INPUT))
  (newline)
  (newline)
  (RUN))

(MAIN)

; check after how many relations the "empty" knowledge starts
; - assuming always it is not "fully gone".
; (define (cnt lis counter)
; (if (equal? (car lis) '(0 0 0)) counter (cnt (cdr lis) (+ 1 counter))))

; gsc -c MAPTRI.SCM && gsc -link MAPTRI.c && gcc MAPTRI.c MAPTRI_.c -lgambc -lm -ldl -lutil -O3 -ffast-math -funroll-loops -floop-interchange -floop-strip-mine -floop-block -ftree-parallelize-loops=64 && mv a.out MAPTRI
; HOWEVER - Chez Scheme is now open source and FASTER in INTERPRETED mode than the above!


