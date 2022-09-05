# img2nc

惑星のDEM（数値標高地図）データをNetCDF形式に変換するプログラム

月の地形図を描きたい人のために作成された。


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
3. プロジェクトのTarballをダウンロードする。

   ```bash
   $ wget https://github.com/ShinobuAmasaki/img2nc/archive/refs/tags/v2.0.0.tar.gz
   $ tar xzf v2.0.0.tar.gz
   ```

4. `fpm`でビルドする。

   ```bash
   $ fpm build --compiler mpif90 \
         --flag "-I/<mpi-include-path> -I/<netcdf-fortran-include-path>" \
         --link-flag "-L/<mpi-library-path> -L/<netcdf-fortran-library-path>"
   ```

5. 任意のディレクトリへインストール

	```bash
	$ fpm install --prefix <directory>
	```


### e.g. Ubuntu 22.04 LTS


## 使い方
例として、月の表面に位置するLambartクレーター (西経21度, 北緯26度)を描いてみる。

### DEMデータをダウンロード
まず初めに、描画およびダウンロードする領域を定める。この領域の範囲を経度と緯度で表現し、コマンドライン引数`west/east/south/north`として実行時に指定する。

経度は月の中心を原点として、東向きを正にとり、0から360の範囲で指定する。
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
$ mpiexec -n 3 img2nc -d dat -o out.nc -r 338/341/24/27
```

プロセス数を`-n`または`-np`オプションで指定する。ただしプロセス数は経度の幅の数値よりも小さくすること。


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

このシェルスクリプトを実行すると、以下の画像ファイル(`lambert.png`)が出力される。
GMTの使い方については [GMT Documentation](https://docs.generic-mapping-tools.org/latest/) を参照のこと。

![lambart](https://user-images.githubusercontent.com/100006043/174430799-5b3f654a-1a47-48d0-ac9e-32976f05390c.png)


## Future works
将来、以下の機能を実装する予定である。

- 粗視化処理
- ✅Intelコンパイラによるビルド
- ✅並列処理
- MOLA(火星のDEM)のデータ処理

[^1]: [Institute of Space and Astronautical Science, Japan Aerospace Exploration Agency - 宇宙航空研究開発機構　宇宙科学研究所](https://www.isas.jaxa.jp/)
[^2]: [Generic Mapping Tools](https://www.generic-mapping-tools.org/)

