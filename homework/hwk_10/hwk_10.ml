open Core.Std
open Vectors

module Int_arithmetic : (Comparable with type t = int) = struct
  type t = int
  let to_string = Int.to_string
  let zero = 0
  let add = (+)
  let mul = ( * )
end

module Complex_arithmetic : (Comparable with type t = float*float) = struct
  type t = float*float
  let to_string c = 
  	match c with a,bi -> "("^(Float.to_string a)^"+"^(Float.to_string bi)^"i)"
  let zero = 0.0,0.0
  let add c1 c2 = match c1,c2 with (a,b),(c,d) -> (a +. c),(b +. d)
  let mul c1 c2 = match c1,c2 with (a,b),(c,d) -> (a*.c -. b*.d),(b*.c +. a*.d)
end
	
module Int_vector = Make_vector(Int_arithmetic)
module Complex_vector = Make_vector(Complex_arithmetic)
