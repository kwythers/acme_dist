CDF       
      nj00      ni00      nv00            user_comment      PStandard CCSM3.1/4.0 domain specification file created from CLM inputdata files:   Land_Grid_Dataset000      griddata_0360x0720.nc000   title000      CCSM domain data:000   SVN_url0      �$HeadURL: https://svn-ccsm-models.cgd.ucar.edu/clm2/branch_tags/ccsm4_0_rel_tags/ccsm4_0_rel_03_clm3_7_10/models/lnd/clm/tools/mkdatadomain/addglobal.F90 $0   Conventions0      NCAR-CSM:CF-1.00   mkdatadomain_version_Id0      4$Id: addglobal.F90 13984 2009-01-20 05:54:15Z erik $   source00      $from CLM fraction and griddata files   NCO0      "4.5.5"0   Land_Fraction_Dataset000      fracdata_0360x0720_ORCHIDEE.nc00   mkdatadomain_version      �$HeadURL: https://svn-ccsm-models.cgd.ucar.edu/clm2/branch_tags/ccsm4_0_rel_tags/ccsm4_0_rel_03_clm3_7_10/models/lnd/clm/tools/mkdatadomain/addglobal.F90 $0   source_code0      4$Id: addglobal.F90 13984 2009-01-20 05:54:15Z erik $   history0     IThu Aug 18 14:09:36 2016: ncks -d ni,550,550 -d nj,271,271 /lustre/pfs1/cades-ccsi/proj-shared/project_acme/ACME_inputdata/atm/datm7/domain.clm/domain.360x720_ORCHIDEE0to360.100409.nc /home/dmricciuto/models/ACME/components/clm/tools/clm4_5/pointclm/temp/domain.lnd.1x1pt_US-UMB_navy.nc

04/09/10 15:31:15 slevis:be1105en.ucar.ed000         xv00                      units000      degrees_east   	long_name000      longitude of grid cell vertices0         
<   yv00                      units000      degrees_north000   	long_name000      latitude of grid cell vertices00         
\   frac                   
coordinate00      xc yc000   	long_name000      $fraction of grid cell that is active   units000      unitless   filter10      =error if frac> 1.0+eps or frac < 0.0-eps; eps = 0.1000000E-11000   filter20      Jlimit frac to [fminval,fmaxval]; fminval= 0.1000000E-02 fmaxval=  1.00000000        
|   area                   
coordinate00      xc yc000   	long_name000      $area of grid cell in radians squared   units000      radians2        
�   xc00                   units000      degrees_east   	long_name000      longitude of grid cell center000   bounds00      xv00        
�   yc00                   units000      degrees_north000   	long_name000      latitude of grid cell center   bounds00      yv00        
�   mask                   
coordinate00      xc yc000   	long_name000      land domain mask   note      unitless   comment0      70=ocean and 1=land, 0 indicates that cell is not active0        
�@q3�y��@q5a@N�@q3�y��@q5a@N�@F�A [�@F�A [�@F��(��@F��(��?�      >ɍ���[�@q4�Fs��@Fǧ��&�   