/*
Test ioctl(fd, FIBMAP, &n) - map file logical block number in @n to FS 
block number (will be saved in @n)

Print FS block numbers occupied by file. Usage: fibmap <file name>

NOTE: You can also get file block numbers with *debugfs* command *stat* 

WARNING: FIBMAP doesn't take into account ext2/ext3 indirect blocks? No. 
All is OK - I checked.
*/

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <fcntl.h> // O_RDONLY
#include <linux/fs.h> // For FIBMAP
#include <stdio.h>

int main (int argc, char *argv[])
{
	int fd, blksz, nblks, i, n;
	struct stat stat;

	if (argc != 2) {
		puts ("Print FS block numbers occupied by file. Usage: fibmap <fname>\n");
		return -1;
	}

	fd = open (argv[1], O_RDONLY);

	if (fd < 0) {
		perror ("Failed to open file");
		return -1;
	}

	if (fstat (fd, &stat) < 0) {
		perror ("fstat failed");
		return -1;
	}

	if (ioctl(fd, FIGETBSZ, &blksz) < 0) {
		perror ("Failed to determine block size");
		return -1;
	}

	nblks = (stat.st_size + blksz - 1) / blksz;
	
	printf ("Block size = %d, blocks = %d\n", blksz, nblks);

	for (i = 0; i < nblks; ++i) {
		n = i;
		if (ioctl (fd, FIBMAP, &n) < 0) {
			perror ("Failed to find FS block number");
			return -1;
		}
		printf ("%d ", n);
	}
	putchar('\n');
	
	return 0;
}
