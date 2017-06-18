# tetris

A terminal interface for Tetris

![terminal-gif](./docs/img/play.gif)

## installation

For Mac and Linux, the binaries attached to the GitHub release should suffice.
Here is a quick way to get the one for your OS:
```bash
curl -L https://github.com/samtay/tetris/releases/download/0.1.0/tetris-`uname -s`-`uname-m` | sudo tee /usr/local/bin/tetris
sudo chmod +x /usr/local/bin/tetris
```

For Windows, you have to install from source. First [get stack](https://docs.haskellstack.org/en/stable/README/#how-to-install). Then
```bash
git clone https://github.com/samtay/tetris.git
cd tetris
stack install tetris
```

## screenshots

#### Linux termite - tomorrow night eighties
![linux-tomorrow-night-80s](./docs/img/linux_tomorrow_night_80s.png)
#### Mac terminal - plain
![mac-terminal-plain](./docs/img/mac_plain.png)
#### Mac terminal - grass
![mac-terminal-grass](./docs/img/mac_grass.png)

## todo

1. Leaderboard saved to txt file (requires adding brick viewport for name entry)
and probably wrapping game in a ui state type
2. Use linear V2 instead of tuples.. dummy
3. Consider refactoring (Game -> a) types with State or Reader abstraction
