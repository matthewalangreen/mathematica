#include	<oauth.h>
#include	<stdlib.h>
#include	<string.h>
#include	<assert.h>
#include	<WolframLibrary.h>

#ifdef CLOUDPLATFORM
/* CloudPlatform product consumer key */
#define CLOUD_OBJECT_CONSUMER_KEY "2eda99406683972710abba0cca1686eb04ea9abeb" /*development key*/
#else
/* Mathematica product consumer key */
#define CLOUD_OBJECT_CONSUMER_KEY "69fb0d89b47fe272d50e66d3da95b5cf052f10a1e" /*development key*/
#endif
#define SECRET_BUFFER_LENGTH 100

char * StringBuffer = NULL;
char * CloudObjectStringBuffer = NULL;
char * CloudObject_ConsumerSecret = NULL;

static void setStringBuffer(const char * val) {
	if (StringBuffer != NULL) {
		free(StringBuffer);
	}
	if (val == NULL) {
		return ;
	} else {
		StringBuffer = strdup(val);
	}
}

static void setCloudObjectStringBuffer(const char * val) {
	if (CloudObjectStringBuffer != NULL) {
		free(CloudObjectStringBuffer);
	}
	if (val == NULL) {
		return ;
	} else {
		CloudObjectStringBuffer = strdup(val);
	}
}

static mbool strsame(const char * a, const char * b) {
	return strcmp(a, b) == 0;
}

typedef struct{ /*container to hold user access tokens*/
	char* accessToken;
	char* accessSecret;
} token_data;

static token_data access_data; /*global token storage container*/

EXTERN_C DLLEXPORT mint WolframLibrary_getVersion( ) {
	return WolframLibraryVersion;
}

/* secret = "4cdc11406ec6abf3c538646607a0";
StringJoin@
 Table[StringJoin["CloudObject_ConsumerSecret[", ToString[i - 1], "] = '", StringTake[secret, {i}], "';\n"],
    {i,   StringLength[secret]}]
 *
 */

EXTERN_C DLLEXPORT int WolframLibrary_initialize( WolframLibraryData libData) {
	CloudObject_ConsumerSecret = (char*)malloc(34);
#ifdef CLOUDPLATFORM
	CloudObject_ConsumerSecret[0] = '4';
	CloudObject_ConsumerSecret[1] = 'c';
	CloudObject_ConsumerSecret[2] = 'd';
	CloudObject_ConsumerSecret[3] = 'c';
	CloudObject_ConsumerSecret[4] = '1';
	CloudObject_ConsumerSecret[5] = '1';
	CloudObject_ConsumerSecret[6] = '4';
	CloudObject_ConsumerSecret[7] = '0';
	CloudObject_ConsumerSecret[8] = '6';
	CloudObject_ConsumerSecret[9] = 'e';
	CloudObject_ConsumerSecret[10] = 'c';
	CloudObject_ConsumerSecret[11] = '6';
	CloudObject_ConsumerSecret[12] = 'a';
	CloudObject_ConsumerSecret[13] = 'b';
	CloudObject_ConsumerSecret[14] = 'f';
	CloudObject_ConsumerSecret[15] = '3';
	CloudObject_ConsumerSecret[16] = 'c';
	CloudObject_ConsumerSecret[17] = '5';
	CloudObject_ConsumerSecret[18] = '3';
	CloudObject_ConsumerSecret[19] = '8';
	CloudObject_ConsumerSecret[20] = '6';
	CloudObject_ConsumerSecret[21] = '4';
	CloudObject_ConsumerSecret[22] = '6';
	CloudObject_ConsumerSecret[23] = '6';
	CloudObject_ConsumerSecret[24] = '0';
	CloudObject_ConsumerSecret[25] = '7';
	CloudObject_ConsumerSecret[26] = 'a';
	CloudObject_ConsumerSecret[27] = '0';
	CloudObject_ConsumerSecret[28] = 'f';
	CloudObject_ConsumerSecret[29] = '2';
	CloudObject_ConsumerSecret[30] = '3';
	CloudObject_ConsumerSecret[31] = '2';
#else
    CloudObject_ConsumerSecret[0]  = '4';
    CloudObject_ConsumerSecret[1]  = '8';
    CloudObject_ConsumerSecret[2]  = 'e';
    CloudObject_ConsumerSecret[3]  = 'd';
    CloudObject_ConsumerSecret[4]  = '3';
    CloudObject_ConsumerSecret[5]  = 'd';
    CloudObject_ConsumerSecret[6]  = 'd';
    CloudObject_ConsumerSecret[7]  = '9';
    CloudObject_ConsumerSecret[8]  = 'd';
    CloudObject_ConsumerSecret[9]  = '0';
    CloudObject_ConsumerSecret[10]  = '1';
    CloudObject_ConsumerSecret[11]  = '2';
    CloudObject_ConsumerSecret[12]  = '3';
    CloudObject_ConsumerSecret[13]  = '5';
    CloudObject_ConsumerSecret[14]  = 'd';
    CloudObject_ConsumerSecret[15]  = '4';
    CloudObject_ConsumerSecret[16]  = '1';
    CloudObject_ConsumerSecret[17]  = '8';
    CloudObject_ConsumerSecret[18]  = '3';
    CloudObject_ConsumerSecret[19]  = 'a';
    CloudObject_ConsumerSecret[20]  = 'd';
    CloudObject_ConsumerSecret[21]  = '1';
    CloudObject_ConsumerSecret[22]  = '2';
    CloudObject_ConsumerSecret[23]  = '3';
    CloudObject_ConsumerSecret[24]  = '0';
    CloudObject_ConsumerSecret[25]  = '1';
    CloudObject_ConsumerSecret[26]  = '7';
    CloudObject_ConsumerSecret[27]  = '5';
    CloudObject_ConsumerSecret[28]  = '7';
    CloudObject_ConsumerSecret[29]  = '1';
    CloudObject_ConsumerSecret[30]  = '2';
    CloudObject_ConsumerSecret[31]  = '2';
#endif
	CloudObject_ConsumerSecret[32]  = '&';
	CloudObject_ConsumerSecret[33] = '\0';
		return LIBRARY_NO_ERROR;
}


EXTERN_C DLLEXPORT void WolframLibrary_uninitialize(WolframLibraryData libData) {
	setStringBuffer(NULL);
	setCloudObjectStringBuffer(NULL);
	free(CloudObject_ConsumerSecret);
}

/*
 *
 * Generic OAuth Authentication Utilities
 *
 */
static OAuthMethod getOAuthMethod(const char * method) {
	if (strsame(method, "HMAC")) {
		return OA_HMAC;
	} else if (strsame(method, "RSA")) {
		return OA_RSA;
	} else {
		assert(False);
		return OA_PLAINTEXT;
	}
}

EXTERN_C DLLEXPORT int oSignURL(WolframLibraryData libData, mint Argc, MArgument * Args, MArgument Res) {

	char * url = MArgument_getUTF8String(Args[0]);
	char * authMethod = MArgument_getUTF8String(Args[1]);
	char * httpMethod = MArgument_getUTF8String(Args[2]);
	char * ckey = MArgument_getUTF8String(Args[3]);
	char * csecret = MArgument_getUTF8String(Args[4]);
	char * tkey = MArgument_getUTF8String(Args[5]);
	char * tsecret = MArgument_getUTF8String(Args[6]);
	char * res;

	res = oauth_sign_url2(url,
						  NULL,
						  getOAuthMethod(authMethod),
						  httpMethod,
						  strsame(ckey, "") ? NULL : ckey,
						  strsame(csecret, "") ? NULL : csecret,
						  strsame(tkey, "") ? NULL : tkey,
						  strsame(tsecret, "") ? NULL : tsecret);

	setStringBuffer(res);
	MArgument_setUTF8String(Res, StringBuffer);

	libData->UTF8String_disown(url);
	libData->UTF8String_disown(authMethod);
	libData->UTF8String_disown(httpMethod);
	libData->UTF8String_disown(ckey);
	libData->UTF8String_disown(csecret);
	libData->UTF8String_disown(tkey);
	libData->UTF8String_disown(tsecret);
	if (res != NULL) {
		free(res);
	}

	return StringBuffer == NULL ? LIBRARY_FUNCTION_ERROR : LIBRARY_NO_ERROR;
}

/*
 *
 * 	CloudObject Authentication Utilities
 *
 */


void set_cloud_object_access_key(token_data *tdata, char *key){
	tdata->accessToken = key;
}

void set_cloud_object_access_secret(token_data *tdata, char *secret){
	tdata->accessSecret = secret;
}

char* get_cloud_object_access_key( ){
	char* res =	access_data.accessToken;

	return res;
}

char* get_cloud_object_access_secret( ){
	char* res =	access_data.accessSecret;

	return res;
}

static char* make_cloud_object_access_secret( ){
	char *access_secret = get_cloud_object_access_secret();
	char * buffer;
	
	buffer = (char*)malloc(SECRET_BUFFER_LENGTH);

	strcpy(buffer, CloudObject_ConsumerSecret);
	strcat(buffer, access_secret);/*TODO: check return value */
	
	setCloudObjectStringBuffer(buffer);
	free(buffer);

	return CloudObjectStringBuffer;

}

static char* get_formatted_cloud_object_secret(const char * secret_type) {
	char* secret;

	if (strsame(secret_type, "Consumer")) {
		secret = CloudObject_ConsumerSecret;
		return secret;
	} else if (strsame(secret_type, "Access")) {
		secret = make_cloud_object_access_secret( );
		return secret;
	} else {
		assert(False);
		return "False";
	}
}

EXTERN_C DLLEXPORT int cloud_object_oauth_hmac(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res)
{
	char *text = MArgument_getUTF8String(Args[0]);
	char *secret = MArgument_getUTF8String(Args[1]);
	char *shasecret, *sign;

	shasecret = get_formatted_cloud_object_secret(secret);

	sign = oauth_sign_hmac_sha1(text, shasecret);

	setCloudObjectStringBuffer(sign);

	MArgument_setUTF8String(Res, CloudObjectStringBuffer);

	libData->UTF8String_disown(text);//prevent memory-leak
	libData->UTF8String_disown(secret);
	if (sign != NULL) {
		free(sign);
	}

	return CloudObjectStringBuffer == NULL ? LIBRARY_FUNCTION_ERROR : LIBRARY_NO_ERROR;

}

EXTERN_C DLLEXPORT int get_consumer_key(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res)
{
	char* string = CLOUD_OBJECT_CONSUMER_KEY;

	MArgument_setUTF8String(Res, string);
	return LIBRARY_NO_ERROR;
}

EXTERN_C DLLEXPORT int set_access_data(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res)
{
	char *key = MArgument_getUTF8String(Args[0]);
	char *secret = MArgument_getUTF8String(Args[1]);

	set_cloud_object_access_key(&access_data, key);
	set_cloud_object_access_secret(&access_data, secret);

	return LIBRARY_NO_ERROR;
}

EXTERN_C DLLEXPORT int get_access_key(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res)
{
	char *key = get_cloud_object_access_key();

	MArgument_setUTF8String(Res,key);
	return LIBRARY_NO_ERROR;
}

EXTERN_C DLLEXPORT int get_access_secret(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res)
{
	char *secret = get_cloud_object_access_secret();

	MArgument_setUTF8String(Res,secret);
	return LIBRARY_NO_ERROR;
}
