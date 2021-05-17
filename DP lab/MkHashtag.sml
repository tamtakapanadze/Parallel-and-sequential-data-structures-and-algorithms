functor MkHashtag(structure Seq : SEQUENCE
                  structure Dict : DICTIONARY) : HASHTAG =
struct
  structure Seq = Seq
  open Seq
  structure Dict = Dict
  open Dict

  (* Begin Cost Justification *)   
  (* End Cost Justification *)
 


 (* val isValidHashtag : string -> bool
  * Given a string S of length n, return true if S can be split into a sequence of valid 
  English words and false otherwise. You may use the function isWord(s), which looks the 
  word s up in a dictionary (insupport/words) and returns a Boolean indicating whether it 
  is a valid word. isWord is case insensitive, so isWord “PseUdoNYm” is equivalent to 
  isWord “pseudonym”.
  *)
 fun isValidHashtag S = 
    if String.size(S) = 0 then false
    else if isWord(S) then true
    else
      let
        val n = String.size S
        val partitions = Seq.tabulate (fn i => 
           (String.substring(S,0,i), String.substring(S,i, n - i))) n
        val words = Seq.map (fn(a, b) => isValidHashtag a 
                                andalso isWord b) partitions
        val answer = Seq.reduce (fn (a, b) => a orelse b) false words
      in
        answer
      end



end

