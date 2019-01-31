wget https://github.com/sosy-lab/benchexec/releases/download/1.17/benchexec_1.17-1_all.deb 2>&1

echo ae | sudo -S apt-get install -y python3-tempita python3-yaml 2>&1
echo ae | sudo -S dpkg -i benchexec_*.deb 2>&1

echo ae | sudo -S adduser cav benchexec

mkdir solvers
cd solvers

cd /home/cav/q3b-artifact/solvers
mkdir Z3
cd Z3
wget https://github.com/Z3Prover/z3/releases/download/z3-4.8.4/z3-4.8.4.d6df51951f4c-x64-ubuntu-16.04.zip
unzip z3-4.8.4.d6df51951f4c-x64-ubuntu-16.04.zip
mv z3-4.8.4.d6df51951f4c-x64-ubuntu-16.04 z3

z3/bin/z3 --version

echo ae | sudo -S cp z3/bin/libz3.{a,so} /usr/lib/
echo ae | sudo -S cp z3/include/* /usr/include/

cd ..

cd /home/cav/q3b-artifact/solvers
mkdir Boolector
cd Boolector
wget https://github.com/Boolector/boolector/archive/3.0.0.zip
unzip 3.0.0.zip
cd ..

wget https://github.com/Kitware/CMake/releases/download/v3.13.3/cmake-3.13.3-Linux-x86_64.tar.gz
tar -xf cmake-3.13.3-Linux-x86_64.tar.gz
echo ae | sudo -S cp -r cmake-3.13.3-Linux-x86_64/bin /usr/
echo ae | sudo -S cp -r cmake-3.13.3-Linux-x86_64/share /usr/
echo ae | sudo -S cp -r cmake-3.13.3-Linux-x86_64/doc /usr/share/
echo ae | sudo -S cp -r cmake-3.13.3-Linux-x86_64/man /usr/share/
echo ae | sudo -S rm -r cmake-3.13.3-Linux-x86_64
echo ae | sudo -S rm cmake-3.13.3-Linux-x86_64.tar.gz
echo ae | sudo -S apt-get install -y uuid-dev

cd Boolector/boolector-3.0.0
./contrib/setup-lingeling.sh
./contrib/setup-btor2tools.sh
./configure.sh && cd build && make

bin/boolector --version

cd ../../..

cd /home/cav/q3b-artifact/solvers
mkdir CVC4
cd CVC4
wget https://cvc4.cs.stanford.edu/downloads/builds/x86_64-linux-opt/cvc4-1.6-x86_64-linux-opt
mv cvc4-1.6-x86_64-linux-opt cvc4
chmod a+x cvc4

./cvc4 --version

cd ..

cd /home/cav/q3b-artifact/solvers
mkdir Q3B
cd Q3B
git clone --recurse-submodules https://github.com/martinjonas/Q3B.git
cd Q3B
git checkout dev
git submodule update --init --recursive
cd ..

echo ae | sudo -S add-apt-repository ppa:ubuntu-toolchain-r/test
echo ae | sudo -S apt update
echo ae | sudo -S apt-get install -y autotools-dev automake g++-7 2>&1
echo ae | sudo -S update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-7 60 \
                     --slave /usr/bin/g++ g++ /usr/bin/g++-7
echo ae | sudo -S update-alternatives --config gcc

git clone https://github.com/martinjonas/cudd.git
cd cudd
./configure --enable-silent-rules --enable-obj --enable-shared && make -j4
echo ae | sudo make install
cd ..

echo ae | sudo -S wget https://www.antlr.org/download/antlr-4.7.2-complete.jar -P /usr/share/java
echo ae | sudo -S apt-get install -y openjdk-9-jre-headless

git config --global user.email "cav@cav"
git config --global user.name "cav"

cd /home/cav/q3b-artifact/solvers/Q3B/Q3B
mkdir build
cd build
cmake .. -DANTLR_EXECUTABLE=/usr/share/java/antlr-4.7.2-complete.jar 2>&1

make -j4

./q3b --version

make test

cd /home/cav/q3b-artifact
mkdir benchmarks
cd benchmarks
wget http://smt-lib.loria.fr/zip/BV.zip
unzip BV.zip
rm BV.zip

echo ae | sudo -S apt-get install -y r-base texlive
echo ae | sudo -S apt-get install -y libssl-dev libxml2-dev libcurl4-openssl-dev
