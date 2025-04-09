'''Daniel Jones | daniel_r_jones2@nps.gov | 20240605
Original code by Zach_Porteous@ghd - tweaked and retweaked by yours truly
use case: export photos in file folder from FGDB (that was exported from AGOL)
photo name ex: REDW_FQUGA 27_0P-50p_04YR02_2023
breakdown: Park _ plotNum _ photoName _ READ _ Year'''

import arcpy, os, sys, re

######## USER INPUT #############
# first script param is the _ATTACH table
inTable = r"N:\GIS_Maps\user_maps\390_FireEcologyandEffects\KLMN_FireEffects_dataDev\Data\GDB\whis_boulder07_240607.gdb\KLMN_FE_Photo_Mon_v1_1__ATTACH"
# field
TJoinField = "REL_GLOBALID"
# Join Feature class
inFC = r"N:\GIS_Maps\user_maps\390_FireEcologyandEffects\KLMN_FireEffects_dataDev\Data\GDB\whis_boulder07_240607.gdb\KLMN_FE_Photo_Mon_v1_1"
# join FC field
FCJoinField = "globalid"
# second script param is the output DIR
fileLocation = r"N:\GIS_Maps\user_maps\390_FireEcologyandEffects\KLMN_FireEffects_dataDev\Images\whis_boulder07_240607"
####### END USER INPUT ###########

arcpy.AddMessage(TJoinField)

t=os.path.split(inTable)[-1]
f=os.path.split(inFC)[-1]


joined = arcpy.AddJoin_management(inTable, TJoinField, inFC, FCJoinField)


with arcpy.da.SearchCursor(joined,[t+'.data',f+'.park_name',f+'.plot_name',t+'.att_name',f+'.plot_read',f+'.reg_date',f+'.photo_4_1_note',f+'.photo_4_2_note',f+'.photo_4_3_note',f+'.photo_4_4_note']) as cursor:
    for row in cursor:
        u="_"
        park = str(row[1])
        plot = str(row[2])
        att = str(row[3])
        ext= att[-4:]
        read = str(row[4])
        date = str(row[5])
        year = date[-4:]

        p = att
        n = att[6]
        
        if n == '8':
            if len(p)==33:
               dir = p[8:13]
            if len(p)==34:
               dir = p[8:14]

        if n == '4':
            num = att[8]
            if num == '1':
                d = str(row[6])
                #dir = num+u+d
            if num == '2':
                d = str(row[7])
                #dir = num+u+d
            if num == '3':
                d = str(row[8])
                #dir = num+u+d
            if num == '4':
                d = str(row[9])
                #dir = num+u+d
            dir = re.sub(":","",d)

        if n == '2':
            dir = p[8:14]

        fname=park+'_'+plot+'_'+dir+'_'+read+'_'+year+ext
        open(os.path.join(fileLocation,fname), "wb").write(row[0].tobytes())

'''photo name ex: REDW_FQUGA 27_0P-50p_04YR02_2023
breakdown: Park _ plotNum _ photoName _ READ _ Year
park + plot + dir + '''


'''with arcpy.da.SearchCursor(joined,[FCname+'.objectid',Tname+'.data',Tname+'.att_name',Tname+'.attachmentid']) as cursor:
    for row in cursor:
        ext=row[2].split('.')[1]
        fname='OID-'+str(row[0])+'-'+str(row[2])
        open(os.path.join(fileLocation,fname), "wb").write(row[1].tobytes())'''
