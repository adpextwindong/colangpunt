# Oct 5th 2023

- Read SCUMM manual


READING:

- https://robertodip.com/blog/scumm-internals-syntax/
- https://web.archive.org/web/20160721004826/http://www.wilmunder.com/Arics_World/Games_files/SCUMM%20Tutorial%200.1.pdf
- https://www.gamedeveloper.com/design/the-scumm-diary-stories-behind-one-of-the-greatest-game-engines-ever-made
- https://www.pagetable.com/?p=614
- https://grumpygamer.com/

- Language work
  - Lexer
  - Parser
  - Asset/script reference checking

- Language constructs
  - Sleep (Use another queue)
    - enqueueAt into sleep queue
  - Spawn (schedules another script)
  - builtins (see [Co #2](https://abhinavsarkar.net/posts/implementing-co-2/))
  - arithmetic
  - control flow

- Game engine integration

  - Text

  - Inventory
    - Up/Down Buttons

  - Verb UI
    - Hoverable texts
      - traverse scene elements for bounding box in order of Z layer

  - State monad manipulation thru an interface

  - Room system (like LOOM)
    - Background rendering
    - Panning logic?

  - Actor system
    - Animation
      - Texture sheet w/ time delta piping into render callsite
    - Walk scripting

  - Object system

  - Verb system

  - Dialog system

  - Animation system
    - example: Walking, then idle script starts up again and does idle animation after wait

  - Imgui editor

  - Console System
