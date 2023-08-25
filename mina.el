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

(defmacro mina-optional-type (term)
  "Wrap type description TERM into an option."
  `(list 'choice ,term '(const nil)))

(defcustom mina-bin (mina-work-dir "mina/_build/default/src/app/cli/src/mina.exe")
  "Mina binary path."
  :type 'file
  :group 'mina)

(defcustom mina-config-dir (mina-work-dir ".mina-sandbox/")
  "Mina config directory."
  :type 'file
  :group 'mina)

(defcustom mina-daemon-key (mina-work-dir "keys/node.libp2p-key")
  "Libp2p discovery key for the daemon."
  :type 'file
  :group 'mina)

(defcustom mina-config-file (mina-work-dir "mina-configs/daemon.json")
  "Mina daemon config file."
  :type 'file
  :group 'mina)

(defcustom mina-block-producer-key (mina-work-dir "keys/block-producer.key")
  "Mina block producer key as Base58."
  :type (mina-optional-type '(file :tag "Filename"))
  :group 'mina)

(defcustom mina-snark-worker-key (mina-read-work-file "keys/snark-producer.key.pub")
  "Mina snark worker key as base58."
  :type (mina-optional-type '(string :tag "Base58"))
  :group 'mina)

(defcustom mina-archive-port 3086
  "Port, on which mina archive is listening."
  :type (mina-optional-type '(integer :tag "Port"))
  :group 'mina)

(defcustom mina-proof-level "none"
  "Mina proof level."
  :type 'string
  :options '("none" "check" "full")
  :group 'mina)

(defcustom mina-peerlist-url nil
  "Peer list URL."
  :type (mina-optional-type '(string :tag "URL"))
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
  "Name of the database for the mina archive to connect to Postgres."
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


(defun mina-process (name cmd)
  "Create an async process NAME running CMD."
  (make-process :name name :buffer (current-buffer) :connection-type 'pipe :command cmd))

(defun mina-import-keys (from to)
  "Import keys from FROM directory to TO config dir."
  (dolist (key (f-files from (lambda (file) (f-ext? file "key"))))
    (call-process mina-bin nil (current-buffer) nil
                  "accounts" "import"
                  "--privkey-path" key
                  "--config-directory" to)))

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

(defun mina-background-process (buffer name program args)
  "Run PROGRAM with ARGS in a process named NAME, attached to BUFFER."
  (save-window-excursion
    (switch-to-buffer buffer)
    (erase-buffer)
    (direnv-update-environment program)
    (mina-process name (cons program args))))

(defmacro mina-run-in-buffer (buffer env &rest body)
  "Run BODY attached to BUFFER, loading ENV."
  `(save-window-excursion
     (switch-to-buffer ,buffer)
     (erase-buffer)
     (direnv-update-environment ,env)
     ,@body))

(defun mina-daemon ()
  "Run Mina daemon."
  (interactive)
  (mina-run-in-buffer
   mina-daemon-buffer-name
   mina-bin
   (delete-directory mina-config-dir t)
   (if mina-sandbox-mode
       (mina-update-genesis-timestamp mina-config-file)
     nil)
   (mina-import-keys (mina-work-dir "keys") mina-config-dir)
   (mina-process "mina-daemon"
                 (append
                  (list mina-bin "daemon"
                        mina-libp2p-flag-name mina-daemon-key
                        "--config-directory" mina-config-dir
                        "--config-file" mina-config-file
                        "--proof-level" mina-proof-level)
                  (mina-unless-null "--block-producer-key" mina-block-producer-key)
                  (mina-unless-null "--run-snark-worker" mina-snark-worker-key)
                  (mina-unless-null "--archive-address" (mina-optmap 'number-to-string mina-archive-port))
                  (mina-unless-null "--peer-list-url" mina-peerlist-url)
                  (if mina-sandbox-mode '("--seed" "--demo-mode") nil)))))

(defun mina-psql-uri ()
  "Return the Postgres URI to connect to."
  (format "postgres://%s@%s:%s/%s"
           mina-archive-db-user
           mina-archive-db-host
           mina-archive-db-port
           mina-archive-db-name))

(defun mina-archive-psql (&rest args)
  "Run psql on the mina archive database with ARGS."
  (apply 'call-process "psql" nil (current-buffer) nil
         "-U" mina-archive-db-user
         "-h" mina-archive-db-host
         "-p" (number-to-string mina-archive-db-port)
         args))

(defun mina-archive ()
  "Start Mina archive."
  (interactive)
  (mina-run-in-buffer
   mina-archive-buffer-name
   mina-archive-bin
   (mina-archive-psql "-d" "postgres"
                      "-c" (format "DROP DATABASE %s;" mina-archive-db-name))
   (mina-archive-psql "-d" "postgres"
                      "-c" (format "CREATE DATABASE %s;" mina-archive-db-name))
   (mina-archive-psql "-d" mina-archive-db-name
                      "-f" (mina-work-dir "mina/src/app/archive/create_schema.sql"))
   (mina-process "mina-archive"
                 (list mina-archive-bin "run"
                       "--postgres-uri" (mina-psql-uri)
                       "--config-file" mina-config-file
                       "--server-port" (number-to-string mina-archive-port)))))

(defun mina-rosetta ()
"Run Rosetta API for Mina."
  (interactive)
  (mina-run-in-buffer
   mina-rosetta-buffer-name
   mina-rosetta-bin
   (setenv "MINA_ROSETTA_MAX_DB_POOL_SIZE" "256")
   (mina-process "mina-rosetta"
                 (list mina-rosetta-bin
                       "--archive-uri" (mina-psql-uri)
                       "--graphql-uri" "http://localhost:3085/graphql"
                       "--port" (number-to-string mina-rosetta-port)
                       "--log-level" "debug"))))

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