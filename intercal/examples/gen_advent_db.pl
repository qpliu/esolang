#!/usr/bin/perl

# Generate INTERCAL from ADVEN.DAT

<>;

print "\tPLEASE INITIALIZE DATABASE DATA\n(61000)\n";

$a = "60001";
$n = "1";
$s = "";
$comment = "PLEASENOT 1\n";
$count = 0;

<>;
while (<>) {
    last unless /^([^\t]*)\t(.*)$/;
    $num = $1;
    $str = $2 . "\n";
    if ($num eq $n) {
	$count++;
	$s = $s . $str;
	$str =~ s/DO/D0/g;
	if ($count == 8 || $count == 16) {
	    $comment = $comment . "THANKYOU  " . $str;
	} else {
	    $comment = $comment . "PLEASENOT " . $str;
	}
    } else {
	print $comment;
	print "THANKS";
	if ($s ne ">\$<\n") {
	    for ($i = 0; $i < length $s; $i++) {
		$j = $i + 1;
		print "DO,${a}SUB#${n}#${j}<-#";
		print ord substr($s,$i);
	    }
	}
	print "\n";
	$n = $num;
	$s = $str;
	$str =~ s/DO/D0/g;
	$comment = "PLEASENOT ${n}\nPLEASENOT " . $str;
	$count = 0;
    }
}

$a = "60002";
while (<>) {
    last unless /^([^\t]*)\t(.*)$/;
    $n = $1;
    $s = $2 . "\n";
    $str = $2;
    $str =~ s/DO/D0/g;
    print "PLEASENOT $n " . $str . "\nTHANKS";
    for ($i = 0; $i < length $s; $i++) {
	$j = $i + 1;
	print "DO,${a}SUB#${n}#${j}<-#";
	print ord substr($s,$i);
    }
    print "\n";
}

$a = "60013";
$b = "60003";
$n = "1";
$index = 1;
while (<>) {
    if ($_ eq "-1\n") {
	<>;
	last;
    }
    @_ = split /\t/;
    $str = $_;
    $str =~ s/DO/D0/g;
    print "PLEASENOT ${str}THANKS";
    if ($n ne $_[0]) {
	print "DO,${a}SUB#${n}<-#${index}";
    }
    $n = $_[0];
    $c = $_[1];
    print "DO,${b}SUB#${index}#1<-#${n}";
    $x = $c%1000;
    if ($x < 150) {
	$x += 256;
    } elsif ($x < 300) {
	die "travel table: $_";
    } elsif ($x < 501) {
	$x = $x - 300 + 256*2;
    } else {
	$x = $x - 500 + 256*3;
    }
    $y = ($c - $c%1000)/1000;
    if ($y == 0) {
	$y = 256;
    } elsif ($y == 100) {
	$y = 256*2;
    } elsif ($y < 100) {
	$y = $y + 256*3;
    } elsif ($y < 200) {
	$y = ($y%100) + 256*4;
    } elsif ($y < 300) {
	$y = ($y%100) + 256*5;
    } elsif ($y < 400) {
	$y = ($y%100) + 256*6;
    } elsif ($y < 500) {
	$y = ($y%100) + 256*7;
    } elsif ($y < 600) {
	$y = ($y%100) + 256*8;
    } elsif ($y < 700) {
	$y = ($y%100) + 256*9;
    } elsif ($y < 800) {
	$y = ($y%100) + 256*10;
    } else {
	die "travel table: $_";
    }
    print "DO,${b}SUB#${index}#2<-#${x}DO,${b}SUB#${index}#3<-#${y}";
    for ($i = 2; $i <= $#_ && $i < 8; $i++) {
	$j = $i + 2;
	print "DO,${b}SUB#${index}#${j}<-#$_[$i]";
    }
    $index++;
    if ($#_ >= 8) {
	print "DO,${b}SUB#${index}#1<-#${n}";
	print "DO,${b}SUB#${index}#2<-#${x}DO,${b}SUB#${index}#3<-#${y}";
	for ($i = 8; $i <= $#_; $i++) {
	    $j = $i - 6;
	    print "DO,${b}SUB#${index}#${j}<-#$_[$i]";
	}
	$index++;
    }
}

$a = "60004";
$index = 1;
while (<>) {
    if ($_ eq "-1\n") {
	<>;
	last;
    }
    @_ = split /\t\n/;
    $n = $_[0];
    if ($n < 1000) {
	$n = $n + 256;
    } elsif ($n < 2000) {
	$n = $n%1000 + 256*2;
    } elsif ($n < 3000) {
	$n = $n%1000 + 256*3;
    } else {
	$n = $n%1000 + 256*4;
    }
    $x = 0;
    $y = 0;
    for ($i = 0; $i < length $_[1]; $i++) {
	$c = ord substr($_[1],$i);
	if ($c == 50) {
	    $c = 29;
	} elsif ($c == 34) {
	    $c = 30;
	}
	$c = $c%32;
	$x = $c if $i == 0;
	$x = $x + 32*$c if $i == 1;
	$x = $x + 32*32*$c if $i == 2;
	$y = $c if $i == 3;
	$y = $y + 32*$c if $i == 4;
	print ord substr($s,$i);
    }
    print "PLEASENOT ${_}THANKS";
    print "DO,${a}SUB#${index}#1<-#${x}";
    print "DO,${a}SUB#${index}#2<-#${y}";
    print "DO,${a}SUB#${index}#3<-#${n}";
    print "\n";
    $index++;
}

print "\tDO RESUME #1\n";
