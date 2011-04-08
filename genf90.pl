#!/bin/perl
use strict;

my $outfile;

# defaults 
my @types = qw(text real double int);
my $vtype = {'text' => 'character(len=*)', 
	     'real' => 'real(r4)', 
	     'double' => 'real(r8)',
	     'int'    => 'integer(i4)',
	     'long'   => 'integer(i8)'};
my $itype = {'text' => 100, 
	     'real' => 101, 
	     'double' => 102,
	     'int'    => 103,
	     'long'   => 104};
my $itypename = {'text' => 'TYPETEXT', 
	     'real' =>  'TYPEREAL', 
	     'double' => 'TYPEDOUBLE',
	     'int'    => 'TYPEINT',
	     'long'   =>  'TYPELONG'};
my $mpitype = {'text' => 'MPI_CHARACTER',
	       'real' => 'MPI_REAL4',
	       'double' => 'MPI_REAL8',
	       'int' => 'MPI_INTEGER'};

my @dims =(0..5);


# begin

foreach(@ARGV){
    my $infile = $_;
    usage() unless($infile =~ /(.*.F90).in/);
    $outfile = $1;
    open(F,"$infile") || die "$0 Could not open $infile to read";
    my @parsetext;
    my $cnt=0;
    foreach(<F>){
	$cnt++;
	if(/^\s*contains/i){
	    push(@parsetext,"# $cnt \"$infile\"\n");
	}
	if(/^\s*interface/i){
	    push(@parsetext,"# $cnt \"$infile\"\n");
	}
	if(/^\s*subroutine/i){
	    push(@parsetext,"# $cnt \"$infile\"\n");
	}
	if(/^.*[^d] function/i){
	    push(@parsetext,"# $cnt \"$infile\"\n");
	}

	push(@parsetext,$_);
    }

    close(F);

    my $end;
    my $contains=0;
    my @unit;
    my $unitcnt=0;
    my $date = localtime();
    my $preamble = 
"!===================================================
! DO NOT EDIT THIS FILE, it was generated using $0 
! Any changes you make to this file may be lost
!===================================================\n";
    my @output ;
    push(@output,$preamble);

    my $line;
    my $dimmodifier;
    my $typemodifier;
    my $itypeflag;
    my $typeblock;
    foreach $line (@parsetext){
# skip parser comments
	next if($line =~ /\s*!pl/);

	$itypeflag=1 if($line =~ /{ITYPE}/);

	
        if($contains==0){	
	    if($line=~/\s*!\s*DIMS\s+[\d,]+!*/){
		$dimmodifier=$line;
		next;
	    }
	    if($line=~/\s*!\s*TYPE\s+[^!]+!*$/){
		$typemodifier=$line;
		next;
	    }
	    if($line=~/^\s*type\s+.*\{DIMS\}/i or $line=~/^\s*type\s+.*\{TYPE\}/i){
		$typeblock=$line;
		next;
	    }
	    if($line=~/^\s*end\s+type\s+.*\{DIMS\}/i or $line=~/^\s*end\s+type\s+.*\{TYPE\}/i){
		$line = $typeblock.$line;
		undef $typeblock;
	    }
	    if(defined $typeblock){
		$typeblock = $typeblock.$line;
		next;
	    }
	    if(defined $dimmodifier){
		$line = $dimmodifier.$line;
		undef $dimmodifier ;
	    } 
	    if(defined $typemodifier){
		$line = $typemodifier.$line;
		undef $typemodifier unless($typeblock==1);
	    } 
	    
	    push(@output, buildout($line));
	}
	if(($line =~ /^\s*contains\s*!*/i) or
	  ($line =~ /^\s*!\s*Not a module/i)){
	    $contains=1;
	    next;
	}
	if($line=~/^\s*end module\s*/){
	    $end = $line;
	    last;
	}

	if($contains==1){
	    # first parse into functions or subroutines
            if(! defined($unit[$unitcnt])){
		# Make cpp lines between routines units
		if($line =~ /^\s*\#/){
		    push(@{$unit[$unitcnt]},$line);
		    $unitcnt++;
		    next;
		}
	    }

	       
	    push(@{$unit[$unitcnt]},$line);
	    if($line =~ /\s*end function/i or $line =~ /\s*end subroutine/i){
		$unitcnt++;
	    }

	}
    }
    my $i;
    for($i=0;$i<$unitcnt;$i++){
	my $func = join('',@{$unit[$i]});
	push(@output, buildout($func));
    }
    push(@output,@{$unit[$#unit]}) if($unitcnt==$#unit);
    push(@output, $end);	
    if($itypeflag==1){
	my $str;
	$str.="#include \"dtypes.h\"\n";
#	foreach (keys %$itype){
#	    $str.="#define $itypename->{$_} $itype->{$_}\n";
#	}
	unshift(@output,$str);
    }
    print @output;
}


sub usage{
    die("$0 Expected input filename of the form .*.F90.in");
}

sub buildout{
    my ($func) = @_;
    
    my $outstr;
    my(@ldims, @ltypes);

    if($func=~/\s*!\s*DIMS\s+([\d,]+)\s*/){
	@ldims = split(/,/,$1);
    }else{
	@ldims = @dims;
    }
    if($func=~/\s*!\s*TYPE\s+([^!\s]+)\s*/){
	@ltypes = split(/,/,$1);	
#	print ">$func<>@ltypes<\n";
    }else{
	@ltypes = @types;
    }
    

    if(($func =~ /{TYPE}/ && $func =~ /{DIMS}/) ){
	my ($type, $dims);
	foreach $type (@ltypes){
	    foreach $dims (@ldims){
		my $dimstr;
		for(my $i=1;$i<=$dims;$i++){
		    $dimstr .=':,';
		}
		if(defined $dimstr){
		    $dimstr="($dimstr";
		    chop $dimstr;
		    $dimstr.=')';
		}else{
		    $dimstr='';
		}
		
		my $str = $func;
		$str =~ s/{TYPE}/$type/g;
		$str =~ s/{VTYPE}/$vtype->{$type}/g;
		$str =~ s/{ITYPE}/$itype->{$type}/g;
		$str =~ s/{MPITYPE}/$mpitype->{$type}/g;
		$str =~ s/{DIMS}/$dims/g;
		$str =~ s/{DIMSTR}/$dimstr/g;
		$outstr .= $str;
	    }
	}
    }elsif($func =~ /{DIMS}/){
        my $dims;
	foreach $dims (@ldims){
	    my $dimstr;
	    for(my $i=1;$i<=$dims;$i++){
		$dimstr .=':,';
	    }
	    if(defined $dimstr){
		$dimstr="($dimstr";
		chop $dimstr;
		$dimstr.=')';
	    }else{
		$dimstr='';
	    }
		
	    my $str = $func;
	    $str =~ s/{DIMS}/$dims/g;
	    $str =~ s/{DIMSTR}/$dimstr/g;
	    $outstr .= $str;
	}
    }elsif($func =~ /{TYPE}/){
	my ($type);
	foreach $type (@ltypes){
	    my $str = $func;
	    $str =~ s/{TYPE}/$type/g;
	    $str =~ s/{VTYPE}/$vtype->{$type}/g;
	    $str =~ s/{ITYPE}/$itype->{$type}/g;
	    $str =~ s/{MPITYPE}/$mpitype->{$type}/g;
	    $outstr.=$str;
	}
    }else{
	$outstr=$func;
    }

    return $outstr;
}
