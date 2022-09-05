$pdflatex="xelatex -synctex=1 -interaction=nonstopmode -shell-escape %O %S && cp -v %D %R.pdf";
$pdf_mode = 1;
$dvi_mode = 0;
$postscript_mode = 0;
$pdf_previewer = 'start zathura'; 
$out_dir = "./output"
