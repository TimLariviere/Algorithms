let merge (arr: int array) a b c =
    let n1 = b - a + 1
    let n2 = c - b
    let arr1 = Array.init n1 (fun i -> arr.[a + i])
    let arr2 = Array.init n2 (fun i -> arr.[b + 1 + i])

    let mutable i = 0
    let mutable j = 0
    for k = a to (a + n1 + n2 - 1) do
        if i = n1 then
            arr.[k] <- arr2.[j]
            j <- j + 1
        else if j = n2 then
            arr.[k] <- arr1.[i]
            i <- i + 1
        else if arr1.[i] <= arr2.[j] then
            arr.[k] <- arr1.[i]
            i <- i + 1
        else
            arr.[k] <- arr2.[j]
            j <- j + 1

let rec mergeSortRec arr a c =
    if a <> c then
        let b = (a + c) / 2
        do mergeSortRec arr a b
        do mergeSortRec arr (b+1) c
        do merge arr a b c
    else
        ()

let mergeSort arr =
    do mergeSortRec arr 0 (arr.Length - 1)

let arra = [| 3; 2; 9; 4; 5; 1; 7; 2 |]

let main () =
    mergeSort arra