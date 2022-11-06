namespace Wishes.Shared

open System

module Wishlists =
    type Wishlist = {
        Id: Guid
        Name: string
        Description: string option
        Wishes: Wishes.Wish list
        Token: string
        CreationTime: DateTimeOffset
    }
    
    type NonSensitiveWishlist = {
        Id: Guid
        Name: string
        Description: string option
        Wishes: Wishes.Wish list
        CreationTime: DateTimeOffset
    }
    
    let asNonSensitive (list: Wishlist) : NonSensitiveWishlist =
        {
            Id = list.Id
            Name = list.Name
            Description = list.Description
            Wishes = list.Wishes
            CreationTime = list.CreationTime
        }
    
    let findWish wishId (list: Wishlist) =
        list.Wishes |> List.tryFind (fun wish -> wish.Id = wishId)

    let updateWishIn (list: Wishlist) wish =
        {
            list with
                Wishes = wish :: (list.Wishes |> List.removeFirst (fun w -> w.Id = wish.Id))
        }
        
    let removeWishFrom wish (list: Wishlist) =
        {
            list with
                Wishes = list.Wishes |> List.removeFirst (fun w -> w.Id = wish.Id)
        }

    let removeWishByIdFrom wishId (list: Wishlist) =
        {
            list with
                Wishes = list.Wishes |> List.removeFirst (fun w -> w.Id = wishId)
        }
        