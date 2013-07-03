#!/usr/bin/perl
@arr = ();
while(<>){
	chomp;
	push(arr,$_);
}
$firstline = shift(@arr);
$numdim = split(',',$firstline);
$numelm = @arr;

print("$numdim $numelm\n");
foreach $elm (@arr){
	$elm =~ s/,/ /g;
	print "$elm\n";
}
