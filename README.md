# grabcite

[![CircleCI](https://circleci.com/gh/agrafix/grabcite.svg?style=svg)](https://circleci.com/gh/agrafix/grabcite)

## Building from source

```bash
# install haskell stack
curl -sSL https://get.haskellstack.org/ | sh

# clone the repo
git clone https://github.com/agrafix/grabcite && cd grabcite

# install dependencies and build
stack setup
stack build

# run it
stack exec -- grabcite-datagen --help
```
