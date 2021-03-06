OPAM_DEPENDS="ocamlfind ocamlfind ounit pareto"
APT_DEPENDS="gsl-bin libgsl0-dev liblapack-dev libblas-dev gfortran"
APT_OCAML="ocaml ocaml-native-compilers camlp4-extra opam aspcud curl"

COVERAGE=1
BISECT_TARBALL="http://bisect.sagotch.fr"
#BISECT_TARBALL="http://sagotch.fr/download/bisect-coveralls.tar.gz"

# complete list, although may not be tested
case "$OCAML_VERSION,$OPAM_VERSION" in
  3.12.1,1.0.0) ppa=avsm/ocaml312+opam10 ;; #unsupported ocaml.3.12.1 / opam 1.0.0
  3.12.1,1.1.0) ppa=avsm/ocaml312+opam11 ;; #unsupported ocaml.3.12.1
  3.12.1,1.2.0) ppa=avsm/ocaml312+opam12 ;; #unsupported ocaml.3.12.1
  4.00.1,1.0.0) ppa=avsm/ocaml40+opam10  ;; #unsupported opam.1.0.0 (via gsl)
  4.00.1,1.1.0) ppa=avsm/ocaml40+opam11  ;;
  4.00.1,1.2.0) ppa=avsm/ocaml40+opam12  ;;
  4.01.0,1.0.0) ppa=avsm/ocaml41+opam10  ;; #unsupported opam.1.0.0 (via gsl)
  4.01.0,1.1.0) ppa=avsm/ocaml41+opam11  ;;
  4.01.0,1.2.0) ppa=avsm/ocaml41+opam12  ;;
  4.02.0,1.0.0) ppa=avsm/ocaml42+opam10  ;; #unsupported opam.1.0.0 (via gsl)
  4.02.0,1.1.0) ppa=avsm/ocaml42+opam11  ;;
  4.02.0,1.2.0) ppa=avsm/ocaml42+opam12  ;;
*) echo Unknown $OCAML_VERSION,$OPAM_VERSION; exit 1 ;;
esac

# setup OPAM
sudo add-apt-repository -y ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq $APT_OCAML $APT_DEPENDS
opam init -y
eval `opam config env`
opam update -y
opam install ${OPAM_DEPENDS} -y

# setup ocverall support via modified bisect
if [ 1 -eq $COVERAGE ] ; then
  curl -L ${BISECT_TARBALL} | tar -xzf -
  cd Bisect
  chmod +x configure
  ./configure
  cat Makefile.config
  make all
  sudo make install
  cd ..
fi

# make/test application
if [ 1 -eq $COVERAGE ] ; then
  make coverage
  JOB_ID="$TRAVIS_JOB_ID"
  JSON_FILE="travis-ci-$JOB_ID.json"
  cd _build
  ./test/test.byte -runner sequential -verbose true -display true
  bisect-report -coveralls-property service_job_id $JOB_ID \
    -coveralls-property service_name travis-ci -coveralls $JSON_FILE *.out
  curl -F json_file=@$JSON_FILE https://coveralls.io/api/v1/jobs
  cd ..
else
  make test.byte
  ./test.byte
fi

# install/test application linking
make
make install
make test
./test/test.native
make uninstall

# documentation
make phylocaml.html
#make phylocaml.tex TODO

# run toplevel with phylocaml TODO
#echo "#use \"topfind\";;
#      #require \"phylocaml\";;
#      open Phylocaml.Tree;;" > test.ml
#ocaml test.ml

# run benchmarks of phylocaml TODO
#make and execute each app

# build all sub-projects TODO
#make and execute each app
