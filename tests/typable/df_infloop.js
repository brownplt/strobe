//this shouldn't make the dataflow run in an infinite loop:
/*for (var i=0; i<10; ++i) {
}*/
var i = 0;
while (i < 10) {
  i = -1; 
  //0 and up make it terminate, but -1 doesn't.
  //i = i + number also doesn't terminate..
}

//with i=i:
/*
(14:let %cps0 (ref 0)
  (13:let i %cps0
    (12:fix
     (%break
      (lambda (%cps1) Any -> Any (11:app %end)))
     (10:fix
      (%loop
       (lambda (%cps2 %cps3 %this)
         (Undefined -> DoesNotReturn) * (Any -> DoesNotReturn) * {} -> DoesNotReturn
         (9:let %cps4 (deref i)
           (8:let %cps5 (< %cps4 10)
             (7:if %cps5
               (6:fix
                (%continue
                 (lambda (%cps6) Any -> Any
                   (5:app %loop %cps2 %cps3 %global)))
                (4:let %cps7 (deref i)
                  (3:let %cps8 (set-ref! i %cps7)
                    (2:app %continue #undefined))))
               (1:app %cps2 #undefined))))))
      (0:app %loop %break %uncaught-exception %global)))))*/
      
//with i=i+0:
/*(15:let %cps0 (ref 0)
  (14:let i %cps0
    (13:fix
     (%break
      (lambda (%cps1) Any -> Any (12:app %end)))
     (11:fix
      (%loop
       (lambda (%cps2 %cps3 %this)
         (Undefined -> DoesNotReturn) * (Any -> DoesNotReturn) * {} -> DoesNotReturn
         (10:let %cps4 (deref i)
           (9:let %cps5 (< %cps4 10)
             (8:if %cps5
               (7:fix
                (%continue
                 (lambda (%cps6) Any -> Any
                   (6:app %loop %cps2 %cps3 %global)))
                (5:let %cps7 (deref i)
                  (4:let %cps8 (+ %cps7 0)
                    (3:let %cps9 (set-ref! i %cps8)
                      (2:app %continue #undefined)))))
               (1:app %cps2 #undefined))))))
      (0:app %loop %break %uncaught-exception %global)))))*/