------------------------------- MODULE bridge -------------------------------

EXTENDS Naturals, TLC

People == { "me", "assistant", "janitor", "professor" }
Side == { "left", "right" }
Speed == "me" :> 1 @@ "assistant" :> 2 @@ "janitor" :> 5 @@ "professor" :> 10

Max(x,y) == IF x <= y 
            THEN y
            ELSE x
 
VARIABLES loc, torch, time
Inv == 
    /\ loc \in [People -> Side]
    /\ torch \in Side
    /\ time \in Nat

Cross(p0,p1,d) ==
    /\ loc[p0] = torch
    /\ loc[p1] = torch
    /\ loc' = [loc EXCEPT ![p0] = d, ![p1] = d ]
    /\ torch' = d
    /\ time' = time + Max(Speed[p0],Speed[p1])

Goal == ~ \A x \in People: loc[x] = "right" /\ time <= 16

Init == 
    /\ loc = [ x \in People |-> "left"]
    /\ torch = "left"
    /\ time = 0

Next == 
    \E p0 \in People, p1 \in People, d \in Side:
        Cross(p0,p1,d)

=============================================================================
\* Modification History
\* Last modified Tue Jan 16 01:17:28 EST 2018 by simon
\* Created Tue Jan 16 00:52:12 EST 2018 by simon
