# img2nc

惑星のDEM（数値標高地図）データをNetCDF形式に変換するプログラム

月の地形図を描きたい人のために作成された。

![lambart](https://user-images.githubusercontent.com/100006043/174430799-5b3f654a-1a47-48d0-ac9e-32976f05390c.png)

## 概要
このソフトウェアは次の機能を持つ：
- **SLDEM2013**のDEMデータのダウンロード
- lblファイルを読み込み、画像データを結合
- NetCDFファイルを出力

**SLDEM2013** は高分解能な月面のDEMの一つであり、ISAS/JAXA[^1]により提供されている[SELENE Data Archive](https://darts.isas.jaxa.jp/planet/pdap/selene/index.html.en)より入手可能である。データを利用する前に当該ライセンスを確認し、利用の際には遵守すること。

このソフトウェアプロジェクトは[**Fortran Package Manager (FPM)**](https://github.com/fortran-lang/fpm)により管理されており、ビルドする際にはこれを利用することを勧める。

このソフトウェアは次に依存している：
- GNU Fortranコンパイラ(gfortran)、またはIntel Fortranコンパイラ(ifort, ifx)
- MPIライブラリ (並列実行のため)
- [**Unidata NetCDF Fortranライブラリ**](https://www.unidata.ucar.edu/software/netcdf/)
- wget (DEMデータをダウンロードするため)


### 動作確認
以下のLinux OSにて動作を確認した。
- Ubuntu Server 22.04 LTS (gfortran/OpenMPI, Intel OneAPI)
- Gentoo Linux (gfortran/OpenMPI)


## インストール
1. コンパイラとFPMをインストールする。
2. MPIライブラリおよびNetCDF Fortranライブラリをインストールする。

   gfortranを使用する場合はディストリビューションのパッケージマネージャーでNetCDFライブラリをインストールしてもよい。しかしIntelコンパイラを使用する場合は、`mod`ファイルが非互換のため、自分自身でNetCDFライブラリをビルドする必要がある。

3. プロジェクトのTarballをダウンロードする。

   ```bash
   $ wget https://github.com/ShinobuAmasaki/img2nc/archive/refs/tags/v2.0.0.tar.gz
   $ tar xzf v2.0.0.tar.gz
   ```

4. `fpm`でビルドする。
   `--compiler`フラグで`mpif90`を指定する。
   このとき、`--flag`にヘッダーファイルのあるディレクトリと`--link-flag`にライブラリファイルのあるディレクトリを指定する必要があるかもしれない。

   ```bash
   $ fpm build --compiler mpif90 \
         --flag "-I/<netcdf-fortran-include-path>" \
         --link-flag "-L/<netcdf-fortran-library-path>"
   ```
   
   もしくは環境変数FPM_FCとFPM_FFLAGSとFPM_LDFLAGSに適当な値を書き込み、その後に`fpm build`を実行する。
   
   ```bash
   $ export FPM_FC=mpif90
   $ export FPM_FFLAGS="-I/<netcdf-fortran-include-path>"
   $ export FPM_LDFLAG="-L/<netcdf-fortran-library-path>"

   $ fpm build 
   ```


5. 任意のディレクトリへインストール

	```bash
	$ fpm install --prefix <directory>
	```


### e.g. Ubuntu 22.04 LTS
Ubuntu 22.04 LTSでのインストール例を示す。

``` bash
$ sudo apt install gfortran openmpi-bin libnetcdff-dev

$ locate mpi_f08
/usr/lib/x86_64-linux-gnu/fortran/gfortran-mod-15/openmpi/mpi_f08.mod

$ ldconfig -p | grep libnetcdff
		libnetcdff.so.7 (libc6,x86-64) => /lib/x86_64-linux-gnu/libnetcdff.so.7
        libnetcdff.so (libc6,x86-64) => /lib/x86_64-linux-gnu/libnetcdff.so
```

メモ： ライブラリのパスは"`/lib/x86_64-linux-gnu`"で、modファイルのディレクトリのパスは"`/usr/lib/x86_64-linux-gnu/fortran/gfortran-mod-15/openmpi`"である。


```bash
# FPMを入手してインストール
$ wget https://github.com/fortran-lang/fpm/releases/download/v0.5.0/fpm-0.5.0-linux-x86_64
$ chmod +x fpm-0.5.0-linux-x86_64
# その後環境変数PATHに含まれるディレクトリにシンボリックリンクを貼る。

# ビルド
$ fpm build --compiler mpif90 \
	 --flag "-I/usr/include -I/usr/lib/x86_64-linux-gnu/fortran/gfortran-mod-15/openmpi" \
	 --link-flag "-L/lib/x86_64-linux-gnu"
```

## 使い方（月面、SLDEM2013）
例として、月の表面に位置するLambartクレーター (西経21度, 北緯26度)を描いてみる。

### DEMデータをダウンロード
まず初めに、描画およびダウンロードする領域を定める。この領域の範囲を経度と緯度で表現し、コマンドライン引数`west/east/south/north`として実行時に指定する。

経度は月の中心を原点として、東向きを正にとり、0から360の範囲で指定する。もしくは東向きを正、西向きを負にとり、-180から180の範囲で指定してもよい。
緯度は赤道を基準に、北向きを正・南向きを負にとり、-90から90の範囲で指定する。

シェルスクリプト`dl_sldem2013.sh`を以下のように引数を与えて実行し、データをダウンロードする。

```
$ ./dl_sldem2013.sh -d dat 338/341/24/27
```

ここで、DEMデータを保存するディレクトリは`-d`オプションとともに指示する。

現在のところ、経度0度線を横断する範囲の指定はサポートしていない。


### 実行
次に実行ファイル`img2nc`を実行しよう。

```
$ img2nc -d dat -o out.nc -r 338/341/24/27
```

実行後、NetCDFファイル`out.nc`が出力される。
引数フラグについては`img2nc -h`を実行すると使い方が表示される。

#### 並列処理での実行
`mpiexec`コマンドを使えば、指定したプロセス数で`img2nc`を実行できる。

```
$ mpiexec -n 9 img2nc -d dat -o out.nc -r 338/341/24/27
```

プロセス数を`-n`または`-np`オプションで指定する。ただしプロセス数は読み込むファイルの数（SLDEM2013では経度と緯度のそれぞれの幅の積）よりも小さくすること。上の例では、最大$3 \times 3 = 9$ プロセスで並列入出力が可能である。


### 描画
GMT[^2]などのGISソフトウェアにより、出力された`out.nc`を使って月面の地形図を描画することができる。

```bash
#!/bin/bash
#This script depends on GMT6
dpi=100
gmt set PROJ_ELLIPSOID = Moon
gmt begin lambart png
	gmt basemap -JM8i -R338/341/24/27 -Bafg -BWeSn
	gmt makecpt -Cviridis -T-5000/1000/1000 -Z
	gmt grdgradient out.nc -Ggrad.grd -A310 -Ne0.6
	gmt grdimage out.nc -JM8i -C -E$dpi -Igrad.grd
gmt end
```

このシェルスクリプトを実行すると、冒頭の画像ファイル(`lambert.png`)が出力される。
GMTの使い方については [GMT Documentation](https://docs.generic-mapping-tools.org/latest/) を参照のこと。

# 使い方（火星、MOLA-MEGDR）

バージョン3.1より、火星のDEMもサポートしている。基本的な使い方は月面の場合と同じであるが、こちらは`img2nc-mola`コマンドを使用する。このセクションでは、火星の全球をメルカトル図法で描画するまでの方法について見てみよう。

DEMのダウンロードには`dl_mola-megdr.sh`スクリプトを使用できる。MOLA-MEGDR(meg128)は全球が16の領域に分割されている。バージョン3.1時点のスクリプトでは全件ダウンロードのみが実行可能である。
```
./dl_mola-megdr.sh -d mola-megdr
```

16並列でNetCDFファイルの書き出しを実行する。なお、`-p`オプションで経緯度1度当たりのピクセル数で解像度を指定することが可能である。
```
$ mpiexec -n 16 img2nc-mola -d mola-megdr -o mola.nc -p 16 -180/180/-88/88
```

scriptディレクトリの`mars-global-mercator.sh`を実行すると、GMTを使用して上で出力された`mola.nc`を、メルカトル図法で以下のような画像に出力することができる。
```
./script/mars-global-mercator.sh
```

![Mars-global](https://github.com/ShinobuAmasaki/img2nc/blob/d64e171d2bb1ebde617e00c03fc34523bf31dc6e/mars-global.png?raw=true)



## Future works
将来、以下の機能を実装する予定である。

- ✅ 粗視化処理（バージョン2）
- ✅ 一度当たりの解像度を指定するオプション
- ✅ Intelコンパイラによるビルド（バージョン2）
- ✅ 並列処理（バージョン2）
- ✅ 出力の並列化（バージョン3）
- ✅ 入力のより多い並列化（バージョン3）
- ✅ 負の経度指定（バージョン3）
- 非同期入出力（FortranのAsynchronous I/O）
- ✅ MOLA(火星のDEM)のデータ処理（バージョン3.1）
- ✅ トリミング（MOLAのみ、バージョン3.1）


[^1]: [Institute of Space and Astronautical Science, Japan Aerospace Exploration Agency - 宇宙航空研究開発機構　宇宙科学研究所](https://www.isas.jaxa.jp/)
[^2]: [Generic Mapping Tools](https://www.generic-mapping-tools.org/)

