namespace Wishes.Giraffe

open System
open System.Security.Cryptography

module Utilities =
    let private availableCharacters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890".ToCharArray()

    let generateToken (length: int) =
        let bytes = RandomNumberGenerator.GetBytes(4 * length)
        let randomNumbers =
            bytes
            |> Array.chunkBySize 4
            |> Array.map (fun bytes -> BitConverter.ToInt32(bytes, 0) |> Math.Abs) // converted to positive Int32s
        let indices =
            randomNumbers
            |> Array.map (fun i -> i % availableCharacters.Length)
        let chars =
            indices
            |> Array.map (fun i -> availableCharacters[i])
        String.Join(String.Empty, chars)