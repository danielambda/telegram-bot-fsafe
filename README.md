# telegram-bot-fsafe

Library for building Telegram bots in Haskell based on type safe FSA architecture.

## License

- The majority of this project is licensed under **MIT** (see [`LICENSE`](LICENSE)).
- Portions derived from [telegram-bot-simple](https://github.com/fizruk/telegram-bot-simple/) remain under **BSD-3-Clause** (see [`LICENSE-BSD`](LICENSE-BSD)).
  - For details see [`NOTICE.md`](NOTICE.md)

## Example

[Here](example) you can see example of using this library for builing telegram bot.
To build and run the example with nix use
``` bash
nix build
nix run
```
If you are using cabal, run
``` bash
cabal build telegram-bot-fsafe-example
cabal run telegram-bot-fsafe-example
```
