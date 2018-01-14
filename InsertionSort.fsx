let insertionSort (array : 'a array) =
    for j = 1 to array.Length - 1 do
        let key = array.[j]
        let mutable i = j - 1
        
        while i > 0 && array.[i] > key do
            array.[i + 1] <- array.[i]
            i <- i - 1
        array.[i + 1] <- key
    array
    
let insertionSortDesc (array : 'a array) =
    for j = array.Length - 2 downto 0 do
        let key = array.[j]
        let mutable i = j + 1
        
        while i < array.Length && array.[i] > key do
            array.[i - 1] <- array.[i]
            i <- i + 1
        array.[i - 1] <- key
    array