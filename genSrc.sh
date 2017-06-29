#!/usr/bin/env bash
if [ $# -ne 3 ]; then
  echo "need paramaters" 1>&2
  exit 1
fi

mainFile=./src/main/scala/$1.scala
testFile=./src/test/scala/$1Spec.scala

cat << EOS > $mainFile
object $1 {
  def $2()
}

EOS

echo "gen src: ${mainFile}"

cat << EOS > $testFile
import $1.$2

class $1Spec extends UnitSpec {
  "$2()" should "solve the problem" in {
    assert($2())
  }
  it should "$3" in {
  }
}

EOS

echo "gen test: ${testFile}"
