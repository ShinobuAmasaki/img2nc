#!/bin/bash
download_lbl() {
   file="$1.lbl"
   url="${REPOS_ROOT}/${file}"
   dir="${DATA_ROOT}/"

   if [ -e "$dir/$file" ]; then
      echo "  $file: Already exists."
   else
      echo -n "  $file: "
      # wget --quiet -P $dir $url && echo "Downloaded."
      wget --verbose -P $dir $url
      sleep 1
   fi
}


download_img() {
   #ファイル名の代入
   file="$1.img"
   #URLの生成
   url="${REPOS_ROOT}/${file}"
   #
   dir="${DATA_ROOT}/"

   if [ -e "$dir/$file" ]; then
      echo "  $file: Already exists."
   else
      echo -n "  $file: "
      # wget --quiet -P $dir $url && echo "Downloaded."
      wget --verbose -P $dir $url
      sleep 1
   fi
}

makecode(){
   #DTM_MAP_01_N51E350N50E351SC
   prefix="meg"
   postfix="hb"

   #領域端の数値を入力する
   local west=$1
   local north=$2
   local k=$3

   return="${prefix}${kind}${north}${west}${postfix}"

}

download_loop() {
   #リストファイルにヘッダー情報を書き込む
   # echo "`expr $east - $west`  `expr $north - $south`" > $code_list

   for kind in c r t ; do 
      #西から東へループする。
      for i in 000 090 180 270 ; do 

         # west=`printf '%03d\n' "$i")`
         
         
         #北から南へループする。
         for j in 00n 44n 44s 88n ; do 
            
            #タイルのコードを作成する。
            makecode $i $j $kind

            #$returnにコードが格納されている
            #lblファイルのダウンロード
            download_lbl $return

            # #imgファイルのダウンロード
            download_img $return
         done
      done
   done
}


pre_execution() {
   #データディレクトリの設定
   while getopts d: OPT
   do
      case $OPT in
         #dの引数有オプションについて、存在する場合にFLG_DをTRUEに設定し、引数をVALUE_Dに代入する
         "d" ) FLG_D="TRUE" ; DATA_ROOT="$OPTARG"
      esac
   done

   #DATA_ROOTが/ではじまる絶対パスか
   HEAD=`echo $DATA_ROOT | cut -c 1`
   if [ $HEAD != "/" ]; then
      PWDIR=`pwd`
      DATA_ROOT="$PWDIR/$DATA_ROOT"
   fi

   #DATA_ROOTが有効なディレクトリか
   if [ ! -d $DATA_ROOT ]; then
      #存在しない場合
      mkdir $DATA_ROOT
   fi

}


usage_exit() {
   echo "Usage: $0" 1>&2
   exit 1
}


num_evaluation() {
   #引数が数値かどうか判定する。
   num=$1
   if [[ "$num" =~ ^[-/+0-9]+$ ]]; then
     :
   else
      usage_exit
   fi
}


main() {
   #シェル変数
   REPOS_ROOT="https://pds-geosciences.wustl.edu/mgs/mgs-m-mola-5-megdr-l3-v1/mgsl_300x/meg128"

   # MAIN
   pre_execution $@


   # ダウンロードループの実行
   download_loop
}

main $@
