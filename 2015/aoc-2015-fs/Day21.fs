module AoC2015.Day21

type ItemType =
    | Weapon
    | Armor
    | Ring

type Character = { hp: int; dmg: int; def: int }

type Item =
    { cost: int
      dmg: int
      def: int
      itemType: ItemType }

type Outcome =
    | Win
    | Lose

let weapon cost dmg =
    { itemType = Weapon
      cost = cost
      dmg = dmg
      def = 0 }

let armor cost def =
    { itemType = Armor
      cost = cost
      def = def
      dmg = 0 }

let ring cost dmg def =
    { itemType = Ring
      cost = cost
      def = def
      dmg = dmg }

// Hardcoding is way faster then parsing.
let boss = { hp = 108; dmg = 8; def = 2 }

let player = { hp = 100; dmg = 0; def = 0 }

let shop =
    [| weapon 8 4
       weapon 10 5
       weapon 25 6
       weapon 40 7
       weapon 74 8
       armor 13 1
       armor 31 2
       armor 53 3
       armor 75 4
       armor 102 5
       ring 25 1 0
       ring 50 2 0
       ring 100 3 0
       ring 20 0 1
       ring 40 0 2
       ring 80 0 3 |]

let equip (character: Character) (item: Item) =
    { character with
        dmg = character.dmg + item.dmg
        def = character.def + item.def }

let fight char1 char2 =
    let turnsNeeded (char1: Character) (char2: Character) =
        let dmgChar1 =
            match char1.dmg - char2.def with
            | x when x <= 0 -> 1
            | x -> x

        char2.hp / dmgChar1

    if turnsNeeded char1 char2 <= turnsNeeded char2 char1 then
        Win
    else
        Lose


let run filename =
    fight { hp = 100; dmg = 2; def = 0 } { hp = 100; dmg = 2; def = 3 }
    |> printfn "%A"
