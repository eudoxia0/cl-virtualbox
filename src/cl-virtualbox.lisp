(in-package :cl-user)
(defpackage cl-virtualbox
  (:nicknames :virtualbox)
  (:use :cl)
  (:export :*vboxmanage-path*
           :list-vms
           :list-running-vms
           :find-by-name
           :find-by-uuid
           :create-vm
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
           :start-vm
           :pause-vm
           :resume-vm
           :cold-reboot-vm
           :poweroff-vm
           :create-hd
           :mount-dvd
           :unmount-dvd))
(in-package :cl-virtualbox)

;;; Utils

(defun keyword->str (keyword)
  (string-downcase (symbol-name keyword)))

(defun bool->str (bool)
  (if bool "on" "off"))

;;; Commands

(defparameter *vboxmanage-path* "vboxmanage"
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

(defun parse-list-line (line)
  (multiple-value-bind (match registers)
      (cl-ppcre:scan-to-strings "^\\\"(.+)\\\" \\{(.+)\\}$" line)
    (declare (ignore match))
    (list :uuid (elt registers 1) :name (elt registers 0))))

(defun parse-vm-list (string)
  (mapcar #'(lambda (line) (parse-list-line line))
          (split-by-newlines string)))

;;;; Interface

;;; Listing VMs

(defun list-vms ()
  "Return a list of plists with the :name and :uuid of every virtual machine."
  (parse-vm-list (run-cmd (cmd "list vms"))))

(defun list-running-vms ()
  "Like virtual-machines, but only return the VMs that are running."
  (parse-vm-list (run-cmd (cmd "list runningvms"))))

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

(defun map-vm-ports (name host-port guest-port)
  "Map TCP traffic to `host-port` to `guest-port` in the guest."
  (run-cmd (cmd "modifyvm ~S --natpf1 \",tcp,,~A,,~A\""
                name host-port guest-port)))

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

;;; DVDs

(defun mount-dvd (name path)
  "Mount a DVD to the virtual machine."
  (run-cmd (cmd "modifyvm ~S --dvd ~S" name (namestring path))))

(defun unmount-dvd (name)
  "Remove the DVD from the virtual DVD drive."
  (run-cmd (cmd "modifyvm ~S --dvd none" name)))
