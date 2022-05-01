#!/bin/sh

inm() {
  npm --prefix assets install --save $1
}

idnm() {
  npm --prefix assets install --save-dev $1
}
