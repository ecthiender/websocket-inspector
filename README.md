# websocket-inspector

A simple Haskell TUI to inspect and debug websocket communication. This is a interactive (TUI) websockets client, which can be used to interactively debug websocket communication with a websocket server.

The program is written using the excellent [brick](https://hackage.haskell.org/package/brick) library.

### Usage

```shell
websocket-inspector --url ws://echo.websocket.org/
```

```shell
websocket-inspector --url "wss://gateway.discord.gg/?v=6&encoding=json"
```

### Demo
[![demo](https://asciinema.org/a/352853.png)](https://asciinema.org/a/352853)


![demo-gif](media/websocket-inspector-demo.GIF)

### How to run it

I have not worked on generating the binary on releases, so you would have to build it on your computer.

- Clone the repo
- Make sure you have [stack](https://docs.haskellstack.org/en/stable/README/) installed
- `cd` into this directory and run `stack install`. This will copy the `websocket-inspector` binary into your `PATH`.
