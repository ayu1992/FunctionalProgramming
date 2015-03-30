 let circle_area_v1 diameter = 
         3.1415 *. (diameter/.2.0)**2.0
 ;;

 let circle_area_v2 diameter = 
     let pi = 3.1415 in 
       let radius = diameter /. 2.0  in
         pi *. radius**2.0
 ;;

 let rec product list = 
         match list with
         |[] -> 1
         |x::rest-> x* product rest
 ;;

 let rec sum_diffs list =
         match list with
         |x1::(x2::[]) -> x1-x2
         |x1::x2::rest -> (x1-x2) + sum_diffs (x2::rest)
 ;;

 let distance (x1,y1) (x2,y2) =
         sqrt((x1-.x2)**2.0 +. (y1-.y2)**2.0)
 ;;

 let triangle_perimeter (x1,y1) (x2,y2) (x3,y3) = 
         distance (x1,y1) (x2,y2) +. distance (x1,y1) (x3,y3) 
         +.distance (x2,y2) (x3,y3)
 ;;

 let perimeter list =
         let init = List.hd list 
         in
         let rec perimeter_helper point list =
           match list with
           |(x1,y1)::(x2,y2)::[] -> distance (x1,y1) (x2,y2) +. 
             distance (x2,y2) init 
           |(x1,y1)::(x2,y2)::rest -> distance (x1,y1) (x2,y2) +. 
             perimeter_helper (x2,y2) ((x2,y2)::rest)
         in perimeter_helper init list
;;

