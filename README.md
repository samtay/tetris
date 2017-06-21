# tetris

A terminal interface for Tetris

![terminal-gif](./docs/img/play.gif)

## installation

For Mac and Linux, the binaries attached to the GitHub release should suffice ([instructions](#github-release-binaries)). For other Linux distros, you can try the Linux binary as well but no guarantees. See the other two options, installing from [source](#install-from-source) and [dex](#install-via-dex).

I have not tested Windows, but I do think it should work via dex. Please let me know in an issue if it fails so I can update documentation. Thanks!

#### github release binaries
Here is a quick way to get the one for your OS:
```bash
curl -L https://github.com/samtay/tetris/releases/download/0.1.0/tetris-`uname -s`-`uname -m` -o tetris
chmod +x tetris
sudo mv tetris /usr/local/bin/
```

#### install from source
First [get stack](https://docs.haskellstack.org/en/stable/README/#how-to-install). Then
```bash
git clone https://github.com/samtay/tetris.git
cd tetris
stack install tetris
```

#### install via dex
[dex](https://github.com/dockerland/dex) is a really cool application manager that lets you run executables as docker images, so that you don't need to worry about installing them or their dependencies directly.
```bash
# get dex if you don't have it
curl -L http://get.iceburg.net/dex/latest-0.12.x/dex -o dex
chmod +x dex
sudo mv dex /usr/local/bin

# add tetris as a repo
dex repo add tetris https://github.com/samtay/tetris.git

# install globally (so that you dont need to run `dex run tetris`)
dex install --global tetris

# test it out
tetris
```

## screenshots

#### Linux termite - tomorrow night eighties
![linux-tomorrow-night-80s](./docs/img/linux_tomorrow_night_80s.png)
#### Mac terminal - plain
![mac-terminal-plain](./docs/img/mac_plain.png)

## todo

1. Leaderboard saved to txt file (requires adding brick viewport for name entry)
and probably wrapping game in a ui state type
2. Use linear V2 instead of tuples.. dummy
3. Consider refactoring (Game -> a) types with State or Reader abstraction
