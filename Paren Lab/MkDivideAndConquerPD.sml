functor MkDivideAndConquerPD (structure Seq : SEQUENCE_RESTRICTED)
  :> PAREN_DIST where type 'a seq = 'a Seq.t =
struct
  type 'a seq = 'a Seq.t

  exception NotYetImplemented


(* fun parenMatch : Paren.t Seq.t -> bool
 * returns true if parentheses are matched false otherwise
 *)  	

  fun parenMatch (ps: Paren.t Seq.t): bool =
	let
		fun parenMatch' ps =
			case Seq.splitMid ps of
				Seq.EMPTY => (0,0)
				| Seq.ONE Paren.L => (0,1)
				| Seq.ONE Paren.R => (1,0)
				| Seq.PAIR (a, b) =>
				    let val ((i,j),(k,l)) =
				    	Primitives.par (fn () => parenMatch' a,
				    		            fn () => parenMatch' b)
				    in
				    	if j <= k then (i+k-j,l) 
				    	else (i,l+j-k) 
				    end
	    in parenMatch' ps = (0,0)
	end 


    fun parenDist (parens : Paren.t Seq.t) : int option = 
    if not (parenMatch(parens)) then NONE
    else
    let fun pD (paren : Paren.t Seq.t) = 
        case (Seq.splitMid paren) of 
          Seq.EMPTY => (0, 0, 0, 0, 0, 0)
          | Seq.ONE(Paren.L) => (0, 1, 0, 1, 0, 1)
          | Seq.ONE(Paren.R) => (0, 0, 1, 0, 1, 1)
          | Seq.PAIR(L, R) =>
            (* uo = unmatched open '('
             * uc = unmatched closed ')'
             * lud = leftmost unmatched distance from the right end
             * rud = rightmost unmatched distance from the left end
             *)
          let
            val ((ans1, uo1, uc1, lud1, rud1, lenl),
              (ans2, uo2, uc2, lud2, rud2, lenr)) =
             Primitives.par (fn () => pD L,
                         fn () => pD R)
          in
              if uo1 = uc2 then 
        (Int.max(ans1, Int.max(ans2, (lud1 + rud2))), 
          uo2, 
          uc1, 
          lud2, 
          rud1, 
          (lenr + lenl)) 
              else if (uo1 > uc2) then 
        (Int.max(ans1, ans2), 
          (uo1 - uc2 + uo2), 
          uc1, 
          (lud1 + lenr), 
           rud1, 
          (lenr + lenl))
              else 
        (Int.max(ans1, ans2), 
          uo2, 
          (uc1 + uc2 - uo1), 
          lud2, 
          (rud2 + lenl),
          (lenr + lenl)) 
          end

          val (ans, uo, uc, lud, rud, len) = pD(parens)

        in 
           SOME (ans - 2)
           
    end

end