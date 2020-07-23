# websocket-inspector

A simple Haskell TUI to inspect and debug websocket communication. This is a interactive (TUI) websockets client, which can be used to interactively debug websocket communication with a websocket server.


## Design

### UI

- User starts the program by passing the websocket URL to connect to. Example: `websocket-inspector ws://localhost:2020/some-path`
- This starts a TUI session, the program opens in full screen mode.
- The visible window is split in two. Left-side shows the data frames received from the websocket server. Right-side shows the data frames that are sent by the client.
- At the bottom of the two splits, there is a input box (like how in chat apps like Slack, Discord), where user can type in anything, and press enter. This sends the raw text (entered by the user) to the server. (And ofcourse, it shows up on the right-split).
