#!/bin/sh

cd `dirname $0`

sudo apt-get install sbcl rlwrap git curl gpg
curl -O https://beta.quicklisp.org/quicklisp.lisp
curl -O https://beta.quicklisp.org/quicklisp.lisp.asc
gpg --verify quicklisp.lisp.asc quicklisp.lisp
echo 'Ok? (^C to quit)'
read OK
sbcl --non-interactive --load quicklisp.lisp --eval '(quicklisp-quickstart:install)'

cat >> ~/.sbclrc <<EOF
(require :asdf)
(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
EOF

mkdir -p ~/.local/bin
sbcl --load build-script-core.lisp ~/.local/bin/lisp

cd ~
git clone https://github.com/pve1/small-dot-emacs
ln -s small-dot-emacs/.emacs

mkdir -p ~/.local/share/common-lisp/source
cd ~/.local/share/common-lisp/source
git clone https://github.com/pve1/apprentice.git
git clone https://github.com/pve1/capitalized-export.git
git clone https://github.com/pve1/extensible-inferred-system.git

mkdir lib
cat >> lib/lib.asd <<EOF
(asdf:defsystem #:lib
  :class "extensible-inferred-system:comment-system"
  :defsystem-depends-on (:extensible-inferred-system))
EOF
