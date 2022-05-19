#!/bin/bash

# dl_img 350/352/49/51

# USAGE: parse $1
parse() {
   #区切り文字列の変更する。
   IFS_SAVE=$IFS
   IFS=/

   #引数に設定する。
   arg="$1"
   set $arg

   #経緯度を設定する
   west="$1";  east="$2"
   south="$3"; north="$4"

   #区切り文字を戻す。
   IFS=$IFS_SAVE

}

# USAGE: makecode <west> <north>
makecode(){
   #DTM_MAP_01_N51E350N50E351SC
   prefix="DTM_MAP_01"
   postfix="SC"

   #領域端の数値を入力する。
   local west=$1
   local north=$2
   local east=`expr "$west" + 1`
   local south=`expr "$north" - 1`

   #NS符号の記述
   #北端の記述
   if [ $north -ge 0 ]; then
      local north="$(printf "%02d\n" "$north")"
      local north="N${north}"

   elif [ $north -lt 0 ]; then
      local north="${north#-}"
      local north="$(printf "%02d\n" "$north")"
      local north="S$north"

   fi

   #南端の記述
   if [ $south -ge 0 ]; then
      local south="$(printf "%02d\n" "$south")"
      local south="N${south}"

   elif [ $south -le -1 ]; then
      local south="${south#-}"
      local south="$(printf "%02d\n" "$south")"   #
      local south="S$south"

   fi
   
   #E符号の追加
   local west="E$(printf "%03d\n" "$west")"
   local east="E$(printf "%03d\n" "$east")"

   #return変数に書き込む
   return="${prefix}_${north}${west}${south}${east}${postfix}"
}

download_lbl() {
   file="$2.lbl"
   url="${repos_root}/$1/data/${file}"
   dir="${data_root}"

   if [ -e "$dir/$file" ]; then
      echo "$file: Already exists."
   else
      echo -n "$file: "
      wget --quiet -P $dir $url && echo "Downloaded."
      sleep 1
   fi
}

download_img() {
   #ファイル名の代入
   file="$2.img"
   #URLの生成
   url="${repos_root}/$1/data/${file}"
   #
   dir="${data_root}"

   if [ -e "$dir/$file" ]; then
      echo "$file: Already exists."
   else
      echo -n "$file: "
      wget --quiet -P $dir $url && echo "Downloaded."
      sleep 1
   fi
}

download_loop() {

   echo "`expr $east - $west`  `expr $north - $south`" > $code_list

   #西から東へループする。
   for ((i=$west; i<$east; i++)) {
      lon_dir="lon$(printf "%03d\n" "$i")"
      echo $lon_dir

      #北から南へループする。
      for ((j=$north; j>$south; j--)){
         
         #タイルのコードを作成する。
         makecode $i $j

         #lblファイルのダウンロード
         download_lbl $lon_dir $return

         #imgファイルのダウンロード
         download_img $lon_dir $return

         echo "${data_root}/${return}" >> $code_list
      }
   }  
}

pre_execution() {
   #引数の存在チェック
   if [ $# -eq 0 ]; then
      echo "Argument  <west>/<east>/<south>/<north>  is required." 1>&2
      exit 1
   fi

   # 引数の範囲チェック
   parse $1

   # echo "$west $east $south $north"
   
   if [[ $west -lt 0 || $west -gt $east || $east -gt 360 ]]; then
      echo "Invalid longitude value." 1>&2
      echo "Longitude range is 0 - 360 deg." 1>&2
      exit 1
   fi

   if [[ $north -gt 90 || $south -gt $north || $south -lt -90 ]]; then
      echo "Invalid latitude value." 1>&2
      echo "Latitude range is -90 - 90 deg." 1>&2
      exit 1
   fi 

   #コードリストの初期化
   if [ -e $code_list ]; then
      rm $code_list
   fi
   touch $code_list

   echo 
}

main() {
#シェル変数
data_root="/home/shin/WORK/dat"
repos_root="https://data.darts.isas.jaxa.jp/pub/pds3/sln-l-tc-5-sldem2013-v1.0"
code_list="./sldem2013_code_list"

# MAIN
pre_execution $@


parse $1
download_loop
}

main $@