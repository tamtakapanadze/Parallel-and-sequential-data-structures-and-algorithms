functor MkFinger (structure Tree : BST) :> FINGER where Tree = Tree =
struct
  structure Tree = Tree

  exception NotYetImplemented
  exception HowDidYouGetHere

  (* Redefine this first... *)
  type 'a nail = (Tree.Key.t * 'a * (Tree.Key.t option * Tree.Key.t option) 
                         * 'a Tree.t * 'a Tree.t)
  type 'a finger = ('a nail * 'a nail list)

  (* ... but don't touch this. *)
  type 'a t = 'a finger

  

  (* val lift : 'a finger -> Tree.Key.t * 'a
   * Given a finger f, (lift f ) should return the key and value 
   stored at the node which f points to. 
   *)
  fun lift f =
    let
      val ((k, v, range, l, r), lis) = f
    in
      (k, v)
    end



  (* val root : 'a Tree.t -> 'a finger option
   * Given a tree t, (root t) should return a finger pointing to 
   the root node of t, or NONE if t is empty. 
   *)
  fun root t = 
    case Tree.expose(t) of
      Tree.LEAF => NONE
      |Tree.NODE {key = k,value = v,left = L,right = R} => 
        SOME ((k, v, (NONE, NONE), L, R), [])



(*******************************SEARCH********************************)

  (* if the key falls in the range
   *)
   fun fit (k, from, to) = 
    case (from, to) of
      (NONE, NONE) => true
      |(NONE, SOME t) => (case Tree.Key.compare(k, t) of
                            LESS => true
                            |_ => false)
      |(SOME f, NONE) => (case Tree.Key.compare(k, f) of
                            GREATER => true
                            |_ => false)
      |(SOME f, SOME t) => (
            case (Tree.Key.compare(k, f), Tree.Key.compare(k, t)) of
              (GREATER, LESS) => true
              |_ => false )



  (* val searchFrom : 'a finger -> Tree.Key.t -> 'a finger option
   * Given a finger f pointing at a node n, and a key k, 
   (searchFrom f k) should return a finger pointing to the node 
   containing k, or NONE if k is not in the tree. Your search must
   start at the node n, and must take work proportional to the 
   length of the unique path between n and the node containing k. 
   If k is not in the tree, then use the unique path between n and 
   the leaf in k’s position instead.
   *)
  fun searchFrom f k =
    let
      val ((key, value, (from, to), L, R), lis) = f
      val nail = (key, value, (from, to), L, R)

      (*Countinue search in the right tree*)
      fun rightS (R', nail', lis', key', to', k) =
      case Tree.expose(R') of
              Tree.LEAF => NONE (*No such key*)
              |Tree.NODE {key = k',value = v',left = l,right = r} => 
               searchFrom ((k', v', (SOME key', to'), l, r), nail'::lis') k

      (*Countinue search in the left tree*)
      fun leftS (L', nail', lis', key', from', k) =
      case Tree.expose(L') of
              Tree.LEAF => NONE (*No such key*)
              |Tree.NODE {key = k',value = v',left = l,right = r} => 
               searchFrom ((k', v', (from', SOME key'), l, r), nail'::lis') k

      (*backrtack*)
      fun backtrack f =
        let
          val ((key, value, (from, to), L, R), l) = f
        in
          case l of
            [] => NONE
            |a::listy => searchFrom (a , listy) k
        end

    in
      case Tree.Key.compare(k, key) of
        EQUAL => SOME f
        |GREATER => (if fit (k, from, to) 
                    then rightS (R, nail, lis, key, to, k)
                    else backtrack f)
        |LESS => (if fit (k, from, to)
                 then leftS (L, nail, lis, key, from, k)
                 else backtrack f)        
    end
    
(****************************************************)


  (* val first : 'a Tree.bst -> 'a finger option
   * Given a tree t, (first t) should return a finger pointing to 
   the leftmost node of t, or NONE if t is empty.
   *)
  fun first t = 
    case Tree.expose(t) of
      Tree.LEAF => NONE
      |Tree.NODE {key = k,value = v,left = L,right = R} => (
        let
          fun first'(tree, f) = 
            case Tree.expose(tree) of
              Tree.LEAF => SOME f
              |Tree.NODE {key = K,value = V,left = l,right = r} =>
                let
                  val (m as (key, value, (from, to), L, R), lis) = f
                in
                  first'(l, ((K,V, (from, SOME key), l, r), m::lis))
                end             
        in
          first' (L, ((k, v, (NONE, NONE), L, R), []))
        end)



  (* val next : 'a finger -> 'a finger option
   * Given a finger f , (next f ) should return a finger pointing 
   to the next node in the tree, or NONE if f points to the 
   rightmost node of the tree.
   *)
  fun next f = 
    let
      val ((key, value, (from, to), L, R), lis) = f
    in
      case Tree.expose(R) of
        Tree.LEAF => (case to of
                        NONE => NONE
                        |SOME z => searchFrom f z)
        |Tree.NODE {key = k,value = v,left = L',right = R'} => 
            searchFrom f (#1 (lift(valOf(first R))))
    end
end
