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
