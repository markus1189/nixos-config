#!/usr/bin/env bats
# diffing-images' scripts with the real odiff, magick and poppler, on images
# and PDFs drawn here. Shapes only: the build sandbox has no fonts. Region
# geometries follow the scripts' 8 px blocks plus one block of margin.

bats_require_minimum_version 1.5.0

setup() {
    bats_load_library bats-support
    bats_load_library bats-assert

    IMG="$BATS_TEST_DIRNAME/../diffing-images/scripts/diff-images.sh"
    PDF="$BATS_TEST_DIRNAME/../diffing-images/scripts/diff-pdfs.sh"
    cd "$BATS_TEST_TMPDIR"
    magick -size 400x300 xc:white -fill '#808080' -draw 'rectangle 20,20 120,120' base.png
    magick base.png -fill red -draw 'rectangle 300,200 339,239' square.png
}

@test "pixel-identical files with different bytes are identical" {
    magick base.png -define png:compression-level=9 -set comment x same.png
    run ! cmp -s base.png same.png
    run bash "$IMG" base.png same.png
    assert_success
    assert_output "identical (threshold 0)"
}

@test "distant changes become separate regions, largest first, each with a crop" {
    magick square.png -fill blue -draw 'point 10,290' two.png
    run bash "$IMG" -o out base.png two.png
    assert_failure 1
    assert_line --index 0 --partial "in 2 regions, threshold 0;"
    assert_line --index 1 "region 1: 64x56+288+192 crop out/crop-1.png (2x) colour #FFFFFF -> #FF0000 (1600 px)"
    assert_line --index 2 "region 2: 24x20+0+280 crop out/crop-2.png (7x) colour #FFFFFF -> #0000FF (1 px)"
    [ -f out/crop-1.png ] && [ -f out/crop-2.png ]
}

@test "an exact diff sees a 4/255 grey step that threshold 0.1 calls identical" {
    magick base.png -fill '#848484' -draw 'rectangle 20,20 120,120' grey.png
    run bash "$IMG" base.png grey.png
    assert_failure 1
    assert_line --index 0 --partial "differ: 10201 px"
    run bash "$IMG" -t 0.1 base.png grey.png
    assert_success
    assert_output "identical (threshold 0.1)"
}

@test "JPEG input defaults to threshold 0.1" {
    magick base.png -quality 90 a.jpg
    magick square.png -quality 90 b.jpg
    run bash "$IMG" a.jpg b.jpg
    assert_failure 1
    assert_line --index 0 --partial "in 1 region, threshold 0.1 (JPEG input; -t 0 for exact)"
}

@test "uppercase extensions and BMPs are read through magick" {
    cp square.png SQUARE.PNG
    magick square.png square.bmp
    run bash "$IMG" square.png SQUARE.PNG
    assert_success
    # odiff alone swaps red and blue in BMPs and would report 1600 px here
    run bash "$IMG" square.bmp square.png
    assert_success
}

@test "a size mismatch reports how many top rows match and crops nothing" {
    magick base.png -background yellow -gravity south -splice 0x10 taller.png
    run bash "$IMG" base.png taller.png
    assert_failure 1
    assert_output --partial "size differs: A 400x300, B 400x310; compared by position, the first 300 rows match."
    refute_output --partial "crop"
}

@test "ignore regions may be fractional, swapped or larger than the image" {
    run bash "$IMG" -i '340.5:240.2-299.9:199' base.png square.png
    assert_success
    run bash "$IMG" --ignore '290:190-9999:9999' base.png square.png
    assert_success
    run bash "$IMG" -i 'nonsense' base.png square.png
    assert_failure 2
}

@test "a reused output directory keeps no crops from the previous run" {
    magick square.png -fill blue -draw 'point 10,290' two.png
    run bash "$IMG" -o out base.png two.png
    run bash "$IMG" -o out base.png square.png
    assert_failure 1
    [ -f out/crop-1.png ] && [ ! -e out/crop-2.png ]
}

@test "missing input, bad threshold and unknown options exit 2" {
    run bash "$IMG" base.png nope.png
    assert_failure 2
    run bash "$IMG" -t 2 base.png base.png
    assert_failure 2
    run bash "$IMG" --bogus base.png base.png
    assert_failure 2
}

@test "diff-pdfs reports each page and pages that exist in only one file" {
    magick base.png base.png base.png a.pdf
    magick base.png square.png base.png base.png b.pdf
    run bash "$PDF" --dpi 72 a.pdf b.pdf
    assert_failure 1
    assert_line --index 0 "page count differs: A=3 B=4"
    assert_line --index 1 "p1 identical (threshold 0)"
    assert_line --index 2 --partial "p2 differ:"
    assert_line --index 3 --regexp '^  region 1: .* crop .*/p2/crop-1\.png \('
    assert_line --index 4 "p3 identical (threshold 0)"
    assert_line --index 5 "p4 only in B"
}

@test "diff-pdfs matches pages despite different zero padding (9 vs 10 pages)" {
    magick base.png -duplicate 8 nine.pdf
    magick base.png -duplicate 9 ten.pdf
    run bash "$PDF" --dpi 30 nine.pdf ten.pdf
    assert_failure 1
    assert_line "page count differs: A=9 B=10"
    assert_line "p9 identical (threshold 0)"
    assert_line "p10 only in B"
}

@test "diff-pdfs rejects a file that is not a PDF with exit 2" {
    echo "not a pdf" > fake.pdf
    magick base.png a.pdf
    run bash "$PDF" a.pdf fake.pdf
    assert_failure 2
    assert_output --partial "pdfinfo cannot read fake.pdf"
}
