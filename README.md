# emacs-mina
Run, monitor and interact with mina daemon inside Emacs.

# Instalation
For the moment there's no installation procedure. Just load
`mina.el` into Emacs and you can start using it. A more
robust installation procedure will probably come when the
package matures.

# Requirements
This package uses a couple of custom Emacs packages which
must be installed in order for this one to work. These packages
are:

* `graphql-mode` – for communicating with the node via GraphQL API.
* `direnv` - for loading direnv environments.

# Usage
The package exports several interactive functions:

* `mina-daemon`
* `mina-daemon-with-archive`
* `mina-daemon-with-rosetta`

Each of them starts the Mina daemon in a separate buffer, where
all of its output goes. `direnv` package is responsible for loading
the Nix environment appropriate for each binary. Besides these functions
also do several other things:

* Configuration directory is deleted if it exists, so that old files
  don't interfere with the new run.
* If sandbox mode is enabled, the genesis ledger timestamp in the
  configuration file is automatically updated to the current time.
* Account keys are imported from `keys/` subdir in the working
  directory `/$HOME/work` for the moment.
*  If Archive is being run (`mina-daemon-with-archive` and
  `mina-daemon-with-rosetta`), then the database is cleared and initialised.
  Running the database is not managed by this package; it must be set up
  manually.
  
There is also `mina-full-stop` command, which stops all the mina processes
that are currently running inside Emacs.

# Configuration
The aforementioned commands don't take any arguments. All the command-line
options to all programs are taken from customisation variables. Open your
customisation panel (`M-x customize`) and type in `mina` into the search box
to display all the relevant configuration options. They are documented
and by default are set up to run a sandboxed node. In order to connect to
a real network, at least some of these options must be changed, but connecting
to a mina network is beyond the scope of this document.

