module Synthesis

let abelar a =  (12 < a && a < 3097 && a%2=0) 
                //failwith "Not implemented"

let area b h = 
    match b>=0.0 && h>=0.0 with 
     |true -> b*h/2.0
     |_ -> failwith "Failed. Base and height are negative values."

let zollo n =
    match n<0 with
    |true -> (n - n) + -n
    |false -> n*2

let min a b =
    match a < b with
    |true -> a
    |false -> b

let max a b =
    match a > b with 
    |true -> a
    |false -> b

let ofTime h m s = h*3600 + m*60 + s

let toTime t = 
    match t >= 0 with
    |false -> 0,0,0
    |true ->
    let hours = t/3600
    let remtime = t - (hours*3600)
    let minutes = remtime/60
    let seconds = remtime - (minutes*60)
    hours,minutes,seconds

let digits n =
    let rec digitCount v c =
        match v=0 with
        |true -> c
        |_ -> digitCount (v/10) (c+1)
    match n>0 || n<0 with 
    |false -> 1
    |true -> digitCount n 0

let minmax (num1, num2, num3, num4) =
    let minval = min num1 num2 |> min num3 |> min num4
    let maxval = max num1 num2 |> max num3 |> max num4
    minval, maxval
    //failwith "Not implemented"

let isLeap y =
    match y<1582 with
    |true -> failwith "Failed. Year is less than 1582"
    |false ->
        match y%4=0 && not(y%100=0) with
        |true -> true
        |false ->
            match y%400=0 with 
            |true -> true
            |false -> false
        
    

let month m =
    match m with
        |1 -> ("January",31)
        |2 -> ("February",28)
        |3 -> ("March", 31)
        |4 -> ("April", 30)
        |5 -> ("May", 31)
        |6 -> ("June", 30)
        |7 -> ("July", 31)
        |8 -> ("August", 31)
        |9 -> ("September", 30)
        |10 -> ("October", 31)
        |11 -> ("November", 30)
        |12 -> ("December", 31)
        |_->  failwith "Failed. Not a valid month."


let toBinary b =
    let rec binConvert v s=
        match v=0 with
        |true -> s
        |false -> 
             match v%2 with
             |0 -> binConvert (v/2) ("0"+s)
             |_ -> binConvert (v/2) ("1"+s)
    match b < 0 with
    |true -> failwith "Failed. Negative value is invalid"
    |_ -> 
        match b = 0 with
        |true -> "0"
        |false -> binConvert b ""
    //failwith "Not implemented"

let bizFuzz n =
    let rec div v (acc1, acc2, acc3) =
      match n < v with
      |true -> (acc1,acc2,acc3)
      |false ->
        match v%3=0 && v%5=0 with
        |true -> div (v+1) (acc1 + 1, acc2+1, acc3+1)
        |false ->
            match v%3=0 with
            |true -> div (v+1) (acc1+1, acc2, acc3)
            |false -> 
                match v%5=0 with
                |true -> div (v+1) (acc1, acc2+1, acc3)
                |false -> div (v+1) (acc1, acc2, acc3)
    match n < 1 with
    |true -> (0,0,0)
    |false -> div 1 (0,0,0)




let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"