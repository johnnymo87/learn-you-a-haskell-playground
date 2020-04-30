# Learn You a Haskell for Great Good!

This repository is a place for writing whatever in Haskell while following along with [the book](http://learnyouahaskell.com/). It uses the latest version of Haskell [in a docker container](https://hub.docker.com/_/haskell), with a memory of GHCi history.

## Install
```sh
docker-compose build
```

## Run
* GHCi
  ```sh
  docker-compose run --rm app
  ```
* Bash
  ```sh
  docker-compose run --rm app bash
  ```
