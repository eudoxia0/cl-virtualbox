`cl-virtualbox` is a library that allows you to control VirtualBox from Common
Lisp, by calling the `vboxmanage` command.

# Usage

See the docstrings of exported symbols for more detailed documentation.

~~~lisp
cl-user> (in-package :virtualbox)
#<package "CL-VIRTUALBOX">
virtualbox> (list-vms)
((:uuid "68fdde1a-a009-4c6f-af6e-250ef879e3e7" :name
  "crane_ubuntu_1410013141185_11153")
 (:uuid "b0ebec92-85cb-408c-b32c-c969e6392d5f" :name
  "vm_ubuntu_1410966111913_95217")
 (:uuid "9ad98045-5109-4233-8b82-4c9a49b4cab7" :name
  "trivial-ssh_default_1411575480546_50302")
 (:uuid "5d287f3c-9b17-4a66-990a-80d4db3167ae" :name "windows"))
virtualbox> (list-running-vms)
nil
virtualbox> (find-by-name "crane_ubuntu_1410013141185_11153")
(:uuid "68fdde1a-a009-4c6f-af6e-250ef879e3e7" :name
 "crane_ubuntu_1410013141185_11153")
virtualbox> (find-by-uuid "9ad98045-5109-4233-8b82-4c9a49b4cab7")
(:uuid "9ad98045-5109-4233-8b82-4c9a49b4cab7" :name
 "trivial-ssh_default_1411575480546_50302")
~~~

# License

Copyright (c) 2014 Fernando Borretti (eudoxiahp@gmail.com)

Licensed under the MIT License.
