open System.Text.RegularExpressions

type Character =
    { hp: int
      dmg: int
      def: int
      mana: int
      totalManaSpent: int }

type SpellType =
    | MagicMissiles
    | Drain
    | Shield
    | Poison
    | Recharge
    | HardDebuff

type Spell =
    { cost: int
      spellType: SpellType
      duration: int }

and State =
    { player: Character
      boss: Character
      activeSpells: Spell list }

type Outcome =
    | Win
    | Loss
    | Ongoing

let magicMissiles =
    { cost = 53
      spellType = MagicMissiles
      duration = 0 }

let drain =
    { cost = 73
      spellType = Drain
      duration = 0 }

let shield =
    { cost = 113
      spellType = Shield
      duration = 6 }

let poison =
    { cost = 173
      spellType = Poison
      duration = 6 }

let recharge =
    { cost = 229
      spellType = Recharge
      duration = 5 }

let hardDebuff =
    { cost = 0
      spellType = HardDebuff
      duration = 9999 }

let boss =
    let input = System.IO.File.ReadAllText "input/22.txt"
    let stats = Regex("\d+").Matches input |> Seq.map (_.Value >> int) |> Seq.toArray

    { hp = stats[0]
      dmg = stats[1]
      def = 0
      mana = 0
      totalManaSpent = 0 }

let initialState =
    { player =
        { hp = 50
          dmg = 0
          def = 0
          mana = 500
          totalManaSpent = 0 }
      boss = boss
      activeSpells = [] }

let damage character amount =
    { character with
        hp = character.hp - amount }

let heal character amount = damage character -amount

let applySpell state spell =
    let { player = player; boss = boss } = state

    match spell.spellType with
    | MagicMissiles -> { state with boss = damage boss 4 }
    | Drain ->
        { state with
            player = heal player 2
            boss = damage boss 2 }
    | Shield ->
        { state with
            player = { player with def = 7 } }
    | Poison -> { state with boss = damage boss 3 }
    | Recharge ->
        { state with
            player = { player with mana = player.mana + 101 } }
    | HardDebuff -> { state with player = damage player 1 }

let removeSpell state spell =
    let player = state.player

    match spell.spellType with
    | Shield ->
        { state with
            player = { player with def = 0 } }
    | _ -> state

let tick spell =
    { spell with
        duration = spell.duration - 1 }

let letBossDealDamage state =
    let { player = player; boss = boss } = state

    let damage =
        if boss.dmg - player.def < 1 then
            1
        else
            boss.dmg - player.def

    { state with
        player = { player with hp = player.hp - damage } }

let tickEffects state =
    let state = state.activeSpells |> List.fold applySpell state
    let spells = state.activeSpells |> List.map tick

    let state =
        spells
        |> List.filter (fun s -> s.duration <= 0)
        |> List.fold removeSpell { state with activeSpells = spells }

    let spells = spells |> List.filter (fun s -> s.duration > 0)

    { state with activeSpells = spells }

let checkState state =
    if state.boss.hp <= 0 then Win
    elif state.player.hp <= 0 then Loss
    else Ongoing

let castSpell spell state =
    { state with
        player =
            { state.player with
                mana = state.player.mana - spell.cost
                totalManaSpent = state.player.totalManaSpent + spell.cost }
        activeSpells = spell :: state.activeSpells }

let allSpells = [ magicMissiles; poison; drain; shield; recharge ]

let castableSpells state =
    let isNotActive spell =
        state.activeSpells
        |> List.map (fun s -> s.spellType)
        |> List.contains spell.spellType
        |> not

    let manaSufficient spell = state.player.mana >= spell.cost

    let isCastable spell =
        isNotActive spell && manaSufficient spell

    List.filter isCastable allSpells

// Forgive me.
// Horrible solution, but I'm really tired of this puzzle.
let mutable bestRun = 9999999

let rec search state =
    seq {
        match checkState state with
        | Loss -> ()
        | Win ->
            if state.player.totalManaSpent < bestRun then
                do bestRun <- state.player.totalManaSpent

            yield state.player.totalManaSpent
        | Ongoing ->
            let state = tickEffects state

            match checkState state with
            | Ongoing ->
                for spell in castableSpells state do
                    if state.player.totalManaSpent + spell.cost > bestRun then
                        ()
                    else
                        let state = castSpell spell state

                        match checkState state with
                        | Ongoing ->
                            let state = tickEffects state

                            match checkState state with
                            | Ongoing -> yield! search (letBossDealDamage state)
                            | _ -> yield! search state
                        | _ -> yield! search state
            | _ -> yield! search state
    }

search initialState |> Seq.min |> printfn "Part 1: %i"

bestRun <- 9999999

let part2 =
    { initialState with
        activeSpells = [ hardDebuff ] }

search part2 |> Seq.min |> printfn "Part 2: %i"
