;;; package --- Mina Launcher
;;; Commentary:
;;; Run Mina daemon and client inside Emacs
;;; Code:

(require 'f)
(require 'sql)
(require 'direnv)

(defun mina-work-dir (subdir)
"Return an absolute path of the SUBDIR in the work directory."
(let ((home (getenv "HOME")))
(concat home "/work/" subdir)))

(defun mina-read-work-file (filename)
"Return the trimmed contents of the FILENAME in the work directory."
(string-trim (f-read-text (mina-work-dir filename))))

(defcustom mina-bin (mina-work-dir "mina/_build/default/src/app/cli/src/mina.exe")
  "Mina binary path."
  :type 'file
  :group 'mina)

(defcustom mina-config-dir (mina-work-dir ".mina-sandbox/")
  "Mina config directory."
  :type 'file
  :group 'mina)

(defcustom mina-daemon-key (mina-work-dir "tmux/keys/node.libp2p-key")
  "Libp2p discovery key for the daemon."
  :type 'file
  :group 'mina)

(defcustom mina-config-file (mina-work-dir "tmux/daemon.json")
  "Mina daemon config file."
  :type 'file
  :group 'mina)

(defcustom mina-block-producer-key (mina-work-dir "tmux/keys/block-producer.key")
  "Mina block producer key as Base58."
  :type 'string
  :group 'mina)

(defcustom mina-snark-worker-key (mina-read-work-file "tmux/keys/snark-producer.key.pub")
  "Mina snark worker key as base58."
  :type 'string
  :group 'mina)

(defcustom mina-archive-port 3086
  "Port, on which mina archive is listening."
  :type 'number
  :group 'mina)

(defcustom mina-proof-level "none"
  "Mina proof level."
  :type 'string
  :options '("none" "check" "full")
  :group 'mina)

(defcustom mina-peerlist-url nil
  "Peer list URL."
  :type 'string
  :group 'mina)

(defcustom mina-libp2p-flag-name "--libp2p-keypair"
  "The flag used to set the libp2p discovery keypair for the daemon."
  :type 'string
  :group 'mina)

(defcustom mina-sandbox-mode 't "Whether to run mina in sandbox mode."
  :type 'boolean
  :group 'mina)

(defcustom mina-daemon-buffer-name "*mina-daemon*"
  "Name of the buffer for mina daemon output."
  :type 'string
  :group 'mina)

(defcustom mina-archive-bin (mina-work-dir "mina/_build/default/src/app/archive/archive.exe")
  "Mina archive binary path."
  :type 'file
  :group 'mina)

(defcustom mina-archive-buffer-name "*mina-archive*"
  "Name of the buffer for mina archive output."
  :type 'string
  :group 'mina)

(defcustom mina-archive-db-user "sven"
  "Username for the mina archive to log in to Postgres."
  :type 'string
  :group 'mina)

(defcustom mina-archive-db-host "localhost"
  "Host for the mina archive to connect to Postgres."
  :type 'string
  :group 'mina)

(defcustom mina-archive-db-port 5432
    "Port for the mina archive to connect to Postgres."
    :type 'number
    :group 'mina)

(defcustom mina-archive-db-name "mina_archive"
  "Name o () f the database for the mina archive to connect to Postgres."
  :type 'string
  :group 'mina)

(defcustom mina-rosetta-port 3087
  "Port, on which mina Rosetta API is listening."
  :type 'number
  :group 'mina)

(defcustom mina-rosetta-bin (mina-work-dir "mina/_build/default/src/app/rosetta/rosetta.exe")
  "Mina rosetta binary path."
  :type 'file
  :group 'mina)

(defcustom mina-rosetta-buffer-name "*mina-rosetta*"
  "Name of the buffer for mina rosetta output."
  :type 'string
  :group 'mina)


(defun mina-import-keys (from to)
  "Import keys from FROM directory to TO config dir."
  (dolist (key (f-files from (lambda (file) (f-ext? file "key"))))
          (shell-command
           (format "%s accounts import -privkey-path %s --config-directory %s"
                   mina-bin-key (f-filename key) to))))

(defun mina-unless-null (flag arg)
  "Format a condition checking if ARG is null and prefixing it with FLAG."
  (if arg
      (list flag arg)
    nil))

(defun mina-update-genesis-timestamp (config)
  "Update the genesis timestamp in the CONFIG file."
  (interactive "fFilename:")
    (with-current-buffer (find-file-noselect config)
      (goto-char 0)
      (let* ((json (json-parse-buffer))
             (genesis (gethash "genesis" json)))
        (puthash "genesis_state_timestamp" (format-time-string "%Y-%m-%dT%H:%M:%S%z") genesis)
        (erase-buffer)
        (json-insert json)
        (json-pretty-print (point-min) (point-max))
        (save-buffer))))

(defun mina-optmap (f opt)
  "Map F over OPT."
  (if opt (funcall f opt) nil))

(defun mina-daemon ()
  "Run Mina daemon."
  (interactive)
  (delete-directory mina-config-dir t)
  (mina-import-keys (mina-work-dir "keys") mina-config-dir)
  (with-current-buffer (get-buffer-create mina-daemon-buffer-name) (erase-buffer))
  (if mina-sandbox-mode
      (mina-update-genesis-timestamp mina-config-file)
    nil)
  (direnv-update-environment mina-bin)
  (make-process
   :name "mina-daemon"
   :buffer mina-daemon-buffer-name
   :connection-type 'pipe
   :command (append (list mina-bin "daemon"
                          mina-libp2p-flag-name mina-daemon-key
                          "--config-directory" mina-config-dir
                          "--config-file" mina-config-file
                          "--proof-level" mina-proof-level)
                    (mina-unless-null "--block-producer-key" mina-block-producer-key)
                    (mina-unless-null "--run-snark-worker" mina-snark-worker-key)
                    (mina-unless-null "--archive-address" (mina-optmap 'number-to-string mina-archive-port))
                    (mina-unless-null "--peer-list-url" mina-peerlist-url)
                    (if mina-sandbox-mode '("--seed" "--demo-mode") nil)))
  (display-buffer mina-daemon-buffer-name))

(defun mina-daemon-stop ()
  "Stop the mina daemon if it is running."
  (interactive)
  (delete-process mina-daemon-buffer-name))

(defun mina-psql-uri ()
  "Return the Postgres URI to connect to."
  (format "postgres://%s@%s:%s/%s"
           mina-archive-db-user
           mina-archive-db-host
           mina-archive-db-port
           mina-archive-db-name))

(defun mina-archive-psql (&rest raw-args)
  "Run psql on the mina archive database with RAW-ARGS."
  (interactive "sQuery:")
  (let ((command (format "psql -U %s -h %s -p %s "
                         mina-archive-db-user
                         mina-archive-db-host
                         mina-archive-db-port))
        (args (mapconcat (lambda (x) (format "\"%s\"" x)) raw-args " ")))
    (message "%s" (concat command args))
    (shell-command (concat command args))))

(defun mina-archive ()
  "Start Mina archive."
  (interactive)
  (mina-archive-psql "-d" "postgres"
                     "-c" (format "DROP DATABASE %s;" mina-archive-db-name))
  (mina-archive-psql "-d" "postgres"
                     "-c" (format "CREATE DATABASE %s;" mina-archive-db-name))
  (mina-archive-psql "-d" mina-archive-db-name
                     "-f" (mina-work-dir "mina/src/app/archive/create_schema.sql"))
  (with-current-buffer (get-buffer-create mina-archive-buffer-name) (erase-buffer))
  (direnv-update-environment mina-archive-bin)
  (make-process
   :name "mina-archive"
   :buffer mina-archive-buffer-name
   :connection-type 'pipe
   :command (list mina-archive-bin "run"
                  "--postgres-uri" (mina-psql-uri)
                  "--config-file" mina-config-file
                  "--server-port" (number-to-string mina-archive-port)))
  (display-buffer mina-archive-buffer-name))

(defun mina-rosetta ()
"Run Rosetta API for Mina."
  (interactive)
  (with-current-buffer (get-buffer-create mina-rosetta-buffer-name) (erase-buffer))
  (direnv-update-environment mina-rosetta-bin)
  (setenv "MINA_ROSETTA_MAX_DB_POOL_SIZE" "256")
  (make-process
   :name "mina-rosetta"
   :buffer mina-rosetta-buffer-name
   :connection 'pipe
   :command (list mina-rosetta-bin
                  "--archive-uri" (mina-psql-uri)
                  "--graphql-uri" "http://localhost:3085/graphql"
                  "--port" (number-to-string mina-rosetta-port)
                  "--log-level" "debug"))
  (display-buffer mina-rosetta-buffer-name))

(defun mina-daemon-with-archive ()
  "Run Mina daemon and a connected archive."
  (interactive)
  (mina-daemon)
  (mina-archive))

(defun mina-daemon-with-rosetta ()
  "Run Mina daemon and a connected Rosetta API."
  (interactive)
  (mina-daemon)
  (mina-archive)
  (mina-rosetta))

(defun mina-stop (process &rest processes)
  "Stop Mina PROCESS and optionally also PROCESSES."
  (interactive "sProcess:")
  (let ((ps (delq nil
             (mapcar
              (lambda (p) (get-buffer-process (format "*%s*" p)))
              (cons process processes)))))
    (dolist (proc ps)
      (delete-process proc))))

(defun mina-full-stop ()
  "Stop all Mina processes."
  (interactive)
  (mina-stop "mina-daemon" "mina-archive" "mina-rosetta"))

(provide 'mina)
;;; mina.el ends here
