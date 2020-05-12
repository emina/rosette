## SymTrace Client

This README details development guide for SymTrace client.

### Setup 

Install the [`yarn`](https://classic.yarnpkg.com/en/docs/install) package manager.

Then, run `yarn` on this directory to install all JS dependencies.

You also need Racket, but if you haven't installed it yet, why are you here?

### Development

Run `yarn start` to launch the client in development mode (in one terminal tab), and `racket ../mock-server.rkt` (in another terminal tab) to start a mock server that will supply error trace data without end.

Navigate to http://localhost:1234/?port=8048 to visit the client site.
You can start editing files, and the site should reflect the change upon a refresh.

#### Mock server status

Following are the statuses of the mock server:

- `waiting-for-connection`: the mock server is waiting for the client to contact the server. Visiting http://localhost:1234/?port=8048 would initiate a contact.
- `connected`: the mock server is supplying data. Quiting or refreshing http://localhost:1234/?port=8048 would sever the connection, making the mock server reset its state to `waiting-for-connection` again.

Therefore, one possible development workflow is to edit files, 
refresh once to sever the connection, and refresh again to establish a new connection.

### Production build 

Run `yarn build` to deploy the actual HTML/JS files in the `dist` directory, 
which is where `raco symtrace` will look for.


