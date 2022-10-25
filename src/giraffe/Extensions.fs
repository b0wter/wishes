namespace Wishes.Giraffe.Extensions

module List =
    
    /// Removes the first element of the list that matches the given predicate. Use this if you are sure that there is
    /// only a single matching element (e.g. when looking for an id) to not traverse the whole list
    let removeFirst predicate list =
        let rec step remaining accumulator =
            match remaining with
            | [] -> list
            | head :: tail ->
                if head |> predicate then accumulator @ remaining
                else step tail (head :: accumulator)
        step list []
