(in-package :cl-user)
(defpackage cl-virtualbox
  (:nicknames :virtualbox)
  (:use :cl)
  (:export :*vboxmanage-path*
           :list-vms
           :list-running-vms
           :list-host-only-ifs
           :find-by-name
           :find-by-uuid
           :create-vm
           :import-vm
           :set-vm-memory
           :set-vm-vram
           :set-vm-cpu-count
           :set-vm-acpi
           :set-vm-ioapic
           :set-vm-pae
           :set-vm-longmode
           :set-vm-hpet
           :set-vm-3d-acceleration
           :map-vm-ports
           :configure-network
           :set-vm-ip
           :start-vm
           :pause-vm
           :resume-vm
           :cold-reboot-vm
           :poweroff-vm
           :create-hd
           :attach-hd
           :mount-dvd
           :unmount-dvd
           :send-virtual-input
           :execute))
(in-package :cl-virtualbox)

;;; Utils

(defun keyword->str (keyword)
  (string-downcase (symbol-name keyword)))

(defun bool->str (bool)
  (if bool "on" "off"))

;;; Commands

(defparameter *vboxmanage-path*
  #-(or win32 mswindows) "vboxmanage"
  #+(or win32 mswindows) "VBoxManage.exe"
  "The path to the VBoxManage command.")

(defun cmd (command-format &rest args)
  (concatenate 'string *vboxmanage-path* " "
               (apply #'format (append (list nil command-format)
                                       args))))

(defun run-cmd (command)
  (uiop:run-program command :output :string))

;;; Parser

(defun split-by-newlines (string)
  (cl-ppcre:split "\\r?\\n" string))

(defun split-by-double-newlines (string)
  (cl-ppcre:split "\\r?\\n\\r?\\n" string))

(defun parse-list-line (line)
  (multiple-value-bind (match registers)
      (cl-ppcre:scan-to-strings "^\\\"(.+)\\\" \\{(.+)\\}$" line)
    (declare (ignore match))
    (list :uuid (elt registers 1) :name (elt registers 0))))

(defun parse-vm-list (string)
  (mapcar #'(lambda (line) (parse-list-line line))
          (split-by-newlines string)))

(defun parse-key-value (string)
  (let ((split (cl-ppcre:split ":[^\\w]+" string)))
    (cons (first split) (second split))))

(defun parse-report (string)
  (loop for report in (split-by-double-newlines string) collecting
    (loop for line in (split-by-newlines report) collecting
      (parse-key-value line))))

;;;; Interface

;;; Listing VMs

(defun list-vms ()
  "Return a list of plists with the :name and :uuid of every virtual machine."
  (parse-vm-list (run-cmd (cmd "list vms"))))

(defun list-running-vms ()
  "Like `list-vms`, but only return the VMs that are running."
  (parse-vm-list (run-cmd (cmd "list runningvms"))))

(defun list-host-only-ifs ()
  "List host only interfaces."
  (parse-report (run-cmd (cmd "list hostonlyifs"))))

;;; Finding VMs

(defun find-by-name (name)
  "Find a virtual machine by name."
  (loop for vm in (list-vms) do
    (if (equal name (getf vm :name))
        (return-from find-by-name vm))))

(defun find-by-uuid (uuid)
  "Find a virtual machine by UUID."
  (loop for vm in (list-vms) do
    (if (equal uuid (getf vm :uuid))
        (return-from find-by-uuid vm))))

;;; Creating and Modifying VMs

(defun create-vm (name)
  "Create a new virtual machine named `name`."
  (run-cmd (cmd "createvm --name ~S --register" name)))

(defun import-vm (ovf-path &optional name)
  "Import a virtual machine from an OVF file."
  (run-cmd (if name
               (cmd "import ~S --vsys 0 --vmname ~S"
                    (namestring ovf-path) name)
               (cmd "import ~S" (namestring ovf-path)))))

(defun set-vm-memory (name memory)
  "Set the VM's memory (In megabytes)"
  (run-cmd (cmd "modifyvm ~S --memory ~A" name memory)))

(defun set-vm-vram (name memory)
  "The the VM's video memory (In megabytes)."
  (run-cmd (cmd "modifyvm ~S --vram ~A" name memory)))

(defun set-vm-cpu-count (name count)
  "Set the number of virtual CPUs the VM has."
  (run-cmd (cmd "modifyvm ~S --cpus ~A" name count)))

(defun set-vm-acpi (name state)
  "Turn ACPI support on/off."
  (run-cmd (cmd "modifyvm ~S --acpi ~A" name (bool->str state))))

(defun set-vm-ioapic (name state)
  "Turn IOAPIC support on/off."
  (run-cmd (cmd "modifyvm ~S --ioapic ~A" name (bool->str state))))

(defun set-vm-pae (name state)
  "Enable/disable PAE."
  (run-cmd (cmd "modifyvm ~S --pae ~A" name (bool->str state))))

(defun set-vm-longmode (name state)
  "Enable/disable longmode."
  (run-cmd (cmd "modifyvm ~S --longmode ~A" name (bool->str state))))

(defun set-vm-hpet (name state)
  "Enable/disable the High-Precision Event Timer (HPET)."
  (run-cmd (cmd "modifyvm ~S --hpet ~A" name (bool->str state))))

(defun set-vm-3d-acceleration (name state)
  "Enable/disable 3D acceleration."
  (run-cmd (cmd "modifyvm ~S --accelerate3d ~A" name (bool->str state))))

;;; VM Network Configuration

(defun map-vm-ports (name host-port guest-ip guest-port)
  "Map TCP traffic from `host-port` to `guest-ip:guest-port` in the guest."
  (run-cmd (cmd "modifyvm ~S --natpf1 \",tcp,,~A,~A,~A\""
                name host-port guest-ip guest-port)))

(defun configure-network (name network-name ip-address lower-ip upper-ip
                  &optional (network-mask "255.255.255.0"))
  "The hard way to set an IP address."
  (run-cmd (cmd "hostonlyif ipconfig ~A --ip ~A" network-name ip-address))
  (run-cmd (cmd "dhcpserver add --ifname ~A --ip ~A --netmask ~A --lowerip ~A --upperip ~A"
                network-name ip-address network-mask lower-ip upper-ip))
  (run-cmd (cmd "dhcpserver modify --ifname ~A --enable" network-name))
  (run-cmd (cmd "modifyvm ~S --intnet1 ~S" name network-name)))

(defun set-vm-ip (name network-name ip-address)
  "The easy way to set an IP address."
  (let* ((parsed-ip (usocket:dotted-quad-to-vector-quad ip-address))
         (lower-ip (copy-seq parsed-ip))
         (upper-ip (copy-seq parsed-ip)))
    (setf (elt lower-ip 3) 1
          (elt upper-ip 3) 255)
    (configure-network name network-name ip-address lower-ip upper-ip)))

;;; Controlling VM state

(defun start-vm (name &key (type :headless))
  "Start the virtual machine."
  (run-cmd (cmd "startvm ~S --type=~A" name (keyword->str type))))

(defun pause-vm (name)
  "Pause the virtual machine."
  (run-cmd (cmd "controlvm ~S pause" name)))

(defun resume-vm (name)
  "Resume the virtual machine after pausing it."
  (run-cmd (cmd "controlvm ~S resume" name)))

(defun cold-reboot-vm (name)
  "Reboot the virtual machine."
  (run-cmd (cmd "controlvm ~S reset" name)))

(defun poweroff-vm (name)
  "Power off the virtual machine."
  (run-cmd (cmd "controlvm ~S poweroff" name)))

;;; Hard Drives

(defun create-hd (path &key size (format :vdi))
  "Create a virtual hard drive on `path`, with size `size` (In megabytes) and
type `type` (:vdi by default)."
  (run-cmd (cmd "createhd --filename ~S --size ~A --format ~A"
                (namestring path)
                size
                (string-upcase (keyword->str format)))))

(defun attach-hd (name pathname &optional (controller "SATA Controller")
                                  (device 0) (port 0))
  (run-cmd (cmd "~S --storagectl ~S --device ~A --port ~A --type hdd --medium ~S"
                name controller device port (namestring pathname))))

;;; DVDs

(defun mount-dvd (name path)
  "Mount a DVD to the virtual machine."
  (run-cmd (cmd "modifyvm ~S --dvd ~S" name (namestring path))))

(defun unmount-dvd (name)
  "Remove the DVD from the virtual DVD drive."
  (run-cmd (cmd "modifyvm ~S --dvd none" name)))

;;; Virtual Keyboard Input

;;; See http://www.win.tue.nl/~aeb/linux/kbd/scancodes-1.html
;;; This is horrible

(defparameter +character-scancode-map+
  (alexandria:alist-hash-table
   '((#\1 . "02 82")
     (#\2 . "03 83")
     (#\3 . "04 84")
     (#\4 . "05 85")
     (#\5 . "06 86")
     (#\6 . "07 87")
     (#\7 . "08 88")
     (#\8 . "09 89")
     (#\9 . "0a 8a")
     (#\0 . "0b 8b")
     (#\- . "0c 8c")
     (#\= . "0d 8d")
     (#\q . "10 90")
     (#\w . "11 91")
     (#\e . "12 92")
     (#\r . "13 93")
     (#\t . "14 94")
     (#\y . "15 95")
     (#\u . "16 96")
     (#\i . "17 97")
     (#\o . "18 98")
     (#\p . "19 99")
     (#\[ . "1a 9a")
     (#\] . "1b 9b")
     (#\a . "1e 9e")
     (#\s . "1f 9f")
     (#\d . "20 a0")
     (#\f . "21 a1")
     (#\g . "22 a2")
     (#\h . "23 a3")
     (#\j . "24 a4")
     (#\k . "25 a5")
     (#\l . "26 a6")
     (#\; . "27 a7")
     (#\' . "28 a8")
     (#\` . "29 a9")
     (#\\ . "2b ab")
     (#\z . "2c ac")
     (#\x . "2d ad")
     (#\c . "2e ae")
     (#\v . "2f af")
     (#\b . "30 b0")
     (#\n . "31 b1")
     (#\m . "32 b2")
     (#\, . "33 b3")
     (#\. . "34 b4")
     (#\/ . "35 b5")
     (#\! . "2a 02 aa 82")
     (#\@ . "2a 03 aa 83")
     (#\# . "2a 04 aa 84")
     (#\$ . "2a 05 aa 85")
     (#\% . "2a 06 aa 86")
     (#\^ . "2a 07 aa 87")
     (#\& . "2a 08 aa 88")
     (#\* . "2a 09 aa 89")
     (#\( . "2a 0a aa 8a")
     (#\) . "2a 0b aa 8b")
     (#\_ . "2a 0c aa 8c")
     (#\+ . "2a 0d aa 8d")
     (#\Q . "2a 10 aa 90")
     (#\W . "2a 11 aa 91")
     (#\E . "2a 12 aa 92")
     (#\R . "2a 13 aa 93")
     (#\T . "2a 14 aa 94")
     (#\Y . "2a 15 aa 95")
     (#\U . "2a 16 aa 96")
     (#\I . "2a 17 aa 97")
     (#\O . "2a 18 aa 98")
     (#\P . "2a 19 aa 99")
     (#\{ . "2a 1a aa 9a")
     (#\} . "2a 1b aa 9b")
     (#\A . "2a 1e aa 9e")
     (#\S . "2a 1f aa 9f")
     (#\D . "2a 20 aa a0")
     (#\F . "2a 21 aa a1")
     (#\G . "2a 22 aa a2")
     (#\H . "2a 23 aa a3")
     (#\J . "2a 24 aa a4")
     (#\K . "2a 25 aa a5")
     (#\L . "2a 26 aa a6")
     (#\: . "2a 27 aa a7")
     (#\" . "2a 28 aa a8")
     (#\~ . "2a 29 aa a9")
     (#\| . "2a 2b aa ab")
     (#\Z . "2a 2c aa ac")
     (#\X . "2a 2d aa ad")
     (#\C . "2a 2e aa ae")
     (#\V . "2a 2f aa af")
     (#\B . "2a 30 aa b0")
     (#\N . "2a 31 aa b1")
     (#\M . "2a 32 aa b2")
     (#\< . "2a 33 aa b3")
     (#\> . "2a 34 aa b4")
     (#\? . "2a 35 aa b5"))))

(defparameter +special-scancode-map+
  (alexandria:plist-hash-table
   '(;; Basic
     :tab "0f 8f"
     :enter "1c 9c"
     :backspace "0e 8e"
     :space "39 b9"
     :return "1c 9c"
     :esc "01 81"
     ;; Arrows
     :up "48 c8"
     :down "50 d0"
     :left "4b cb"
     :right "4d cd"
     ;; Page control
     :page-up "49 c9"
     :page-down "51 d1"
     :home "47 c7"
     :end "4f cf"
     :insert "52 d2"
     :delete "53 d3"
     ;; Function keys
     :f1 "3b"
     :f2 "3c"
     :f3 "3d"
     :f4 "3e"
     :f5 "3f"
     :f6 "40"
     :f7 "41"
     :f8 "42"
     :f9 "43"
     :f10 "44")))

(defun string->scancodes (string)
  (format nil "~{~A ~}"
          (loop for char across string collecting
            (gethash char +character-scancode-map+))))

(defun keyword->scancodes (keyword)
  (gethash keyword +special-scancode-map+))

(defun send-scancodes (name scancodes)
  (run-cmd (cmd "controlvm ~S keyboardputscancode ~A" name scancodes)))

(defun send-virtual-input (name input-list)
  (loop for input in input-list do
    (typecase input
      (string
       (send-scancodes name (string->scancodes input)))
      (keyword
       (send-scancodes name (keyword->scancodes input))))))

;;; Execute code

(defun execute (name image username password &key wait-stdout)
  "Execute `image` on the virtual machine `name`, under the account `username`
and `password`. If `wait-stdout` is true, wait for the program to finish and
print its output."
  (run-cmd (cmd "guestcontrol ~S run --exe ~S --username ~S --password ~S ~A"
                name
                image
                username
                password
                (if wait-stdout
                    "--wait-stdout"
                    ""))))
