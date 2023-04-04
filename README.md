# Test Repository for Binder with R 4.2.2

『[私たちのR: ベストプラクティスの探求](https://www.jaysong.net/RBook/)』の学習に必要なパッケージのみ事前インストールしたまっさらなR環境


## Rの使い方

* 宋財泫・矢内勇生『私たちのR: ベストプラクティスの探求』(Web-book): <https://www.jaysong.net/RBook/>
* 関西大学総合情報学部「ミクロ/マクロ政治データ分析実習」のサポートページ: <https://www.jaysong.net/r4ps/>

## 動作環境

```bash
$ echo $BASH_VERSION
4.4.20(1)-release

$ cat /etc/lsb-release
DISTRIB_ID=Ubuntu
DISTRIB_RELEASE=18.04
DISTRIB_CODENAME=bionic
DISTRIB_DESCRIPTION="Ubuntu 18.04.6 LTS"

$ gcc --version
gcc (Ubuntu 7.5.0-3ubuntu1~18.04) 7.5.0
Copyright (C) 2017 Free Software Foundation, Inc.
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

$ R --version
R version 4.2.2 (2022-10-31) -- "Innocent and Trusting"

$ rstudio-server version
2022.12.0+353 (Elsbeth Geranium) for Ubuntu Bionic
```


## 備忘録

**初期設定時**

* 最上位branch名に注意。Binderのデフォルトはmasterだが、mainならbranch名にmainと指定
* 分析環境から初期ファイル（`apt.txt`や`environment.yml`）などを削除したい場合は`postBuild`ファイル内で指定

**日本語作図**

RStudio上で表示は問題ない。ただし、PDFで書き出す際、フォントの埋め込みが必要なのでdeviceをCairoに指定。

* Plot Paneからの操作だとCairo利用にチェック
* `ggsave()`を利用するなら`device = cairo_pdf`を指定

```r
pacman::p_load(tidyverse)

tibble(x = rnorm(10), y = rnorm(10)) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  labs(x = "横軸", y = "縦軸")

ggsave(filename = "Test.pdf", plot = last_plot(), device = cairo_pdf, height = 5, width = 5)
```

PNG出力なら{ragg}で一発解決

* デバイス（`dev`）は`ragg::agg_png`、解像度（`dpi`）は300以上にしておけば、印刷しても綺麗な図ができる。むろん、文字化けの心配もない。

```{r}
ggsave(..., dev = ragg::agg_png, dpi = 300, ...)
```

**プロジェクトを開く**

* Jupyter hubから`.Rproj`を選択しても開かれないため、RStudioを起動し、File > Open Project...で開く必要がある。


## 参考

* [Specifying an R environment with a runtime.txt file](https://github.com/binder-examples/r)
* [From Zero to Binder in R!](https://github.com/alan-turing-institute/the-turing-way/blob/master/workshops/boost-research-reproducibility-binder/workshop-presentations/zero-to-binder-r.md)
* [r-conda](https://github.com/binder-examples/r-conda)
  * こっちの方がDockerイメージ生成が速いらしい
* [オンライン分析システム（実証実験）](https://meatwiki.nii.ac.jp/confluence/display/niircosap)
  * [初期設定](https://binder.cs.rcos.nii.ac.jp/) (学認)
  * [Jupyter hub](https://jupyter.cs.rcos.nii.ac.jp/) (学認)
