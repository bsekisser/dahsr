#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include <locale.h>
#include <wchar.h>

const wchar_t* STRING(uint8_t len, wchar_t c)
{
	wchar_t* dst = malloc(sizeof(wchar_t) * (len + 1));
	for(int i = 0; i < len; i++)
		dst[i] = c;

	return(dst);
}

void main(void)
{
//	int res = setlocale(LC_ALL, "ISO_8859-1");
//	char* locale = setlocale(LC_ALL, "C.UTF-8");
	char* locale = setlocale(LC_ALL, "");
	
	printf("0x%08x\n", locale);
	printf("0x%04x, 0x%04x", 'É', '»');
//	printf("%lc%ls%lc", 0xc389, STRING(78, /*205*/0x2550), 0xc2bb);
	printf("%lc%ls%lc", 0x2554, STRING(78, /*205*/0x2550), 0x2557);
}
