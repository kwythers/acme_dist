CDF      
      lndgrid       gridcell      landunit      column        pft        levgrnd       levurb        levlak     
   numrad        levsno        ltype      	   natpft        string_length         levdcmp       hist_interval         time             title         CLM History file information   comment       :NOTE: None of the variables are weighted by land fraction!     Conventions       CF-1.0     history       created on 05/09/17 02:21:00   source        Community Land Model CLM4.0    hostname      mesabi     username      chen1718   version           revision_id       9$Id: histFileMod.F90 42903 2012-12-21 15:32:10Z muszala $      
case_title        UNSET      case_id       !trait_dist2_US-UMB_I20TRCLM45CBCN      Surface_dataset       "surfdata_1x1pt_US-UMB_simyr1850.nc     Initial_conditions_dataset        <trait_dist2_US-UMB_I1850CLM45CBCN.clm2.r.0211-01-01-00000.nc   #PFT_physiological_constants_dataset       clm_params.c150521.nc      ltype_vegetated_or_bare_soil            
ltype_crop              ltype_landice               (ltype_landice_multiple_elevation_classes            ltype_deep_lake             ltype_wetland               ltype_urban_tbd             ltype_urban_hd              ltype_urban_md           	   Time_constant_3Dvars_filename         ?./trait_dist2_US-UMB_I20TRCLM45CBCN.clm2.h0.1850-01-01-00000.nc    Time_constant_3Dvars      /ZSOI:DZSOI:WATSAT:SUCSAT:BSW:HKSAT:ZLAKE:DZLAKE      ,   levgrnd                	long_name         coordinate soil levels     units         m         <     �p   levlak                 	long_name         coordinate lake levels     units         m         (     ��   levdcmp                	long_name         coordinate soil levels     units         m         <     ��   time               	long_name         time   units         days since 1850-01-01 00:00:00     calendar      noleap     bounds        time_bounds            �,   mcdate                 	long_name         current date (YYYYMMDD)            �0   mcsec                  	long_name         current seconds of current date    units         s              �4   mdcur                  	long_name         current day (from base day)            �8   mscur                  	long_name         current seconds of current day             �<   nstep                  	long_name         	time step              �@   time_bounds                   	long_name         history time interval endpoints            �D   date_written                            �T   time_written                            �\   lon                 	long_name         coordinate longitude   units         degrees_east   
_FillValue        {@��   missing_value         {@��           �   lat                 	long_name         coordinate latitude    units         degrees_north      
_FillValue        {@��   missing_value         {@��           �   area                	long_name         grid cell areas    units         km^2   
_FillValue        {@��   missing_value         {@��           �   topo                	long_name         grid cell topography   units         m      
_FillValue        {@��   missing_value         {@��           �   landfrac                	long_name         land fraction      
_FillValue        {@��   missing_value         {@��           �    landmask                	long_name         &land/ocean mask (0.=ocean and 1.=land)     
_FillValue        ����   missing_value         ����           �$   pftmask                 	long_name         (pft real/fake mask (0.=fake and 1.=real)   
_FillValue        ����   missing_value         ����           �(   ACTUAL_IMMOB                   	long_name         actual N immobilization    units         gN/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �d   ACTUAL_IMMOB_P                     	long_name         actual P immobilization    units         gP/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �h   AGNPP                      	long_name         aboveground NPP    units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �l   ALT                    	long_name         current active layer thickness     units         m      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �p   ALTMAX                     	long_name         %maximum annual active layer thickness      units         m      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �t   ALTMAX_LASTYEAR                    	long_name         )maximum prior year active layer thickness      units         m      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �x   AR                     	long_name         !autotrophic respiration (MR + GR)      units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �|   BAF_CROP                   	long_name         fractional area burned for crop    units         proportion/sec     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   	BAF_PEATF                      	long_name         "fractional area burned in peatland     units         proportion/sec     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   BCDEP                      	long_name         -total BC deposition (dry+wet) from atmosphere      units         kg/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   BGNPP                      	long_name         belowground NPP    units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   BIOCHEM_PMIN                   	long_name         $biochemical rate of P mineralization   units         gP/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   BTRAN                      	long_name         transpiration beta factor      units         unitless   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   	BUILDHEAT                      	long_name         8heat flux from urban building interior to walls and roof   units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   
COL_CTRUNC                     	long_name         "column-level sink for C truncation     units         gC/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   COL_FIRE_CLOSS                     	long_name         Ttotal column-level fire C loss for non-peat fires outside land-type converted region   units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   COL_FIRE_NLOSS                     	long_name         total column-level fire N loss     units         gN/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   COL_FIRE_PLOSS                     	long_name         total column-level fire P loss     units         gP/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   
COL_NTRUNC                     	long_name         "column-level sink for N truncation     units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   
COL_PTRUNC                     	long_name         "column-level sink for P truncation     units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   CPOOL                      	long_name         temporary photosynthate C pool     units         gC/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   CWDC                   	long_name         CWD C      units         gC/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   CWDC_HR                    	long_name         /coarse woody debris C heterotrophic respiration    units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   	CWDC_LOSS                      	long_name         coarse woody debris C loss     units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   CWDC_TO_LITR2C                     	long_name         .decomp. of coarse woody debris C to litter 2 C     units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   CWDC_TO_LITR3C                     	long_name         .decomp. of coarse woody debris C to litter 3 C     units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   CWDC_vr                       	long_name         CWD C (vertically resolved)    units         gC/m^3     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     ��   CWDN                   	long_name         CWD N      units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   CWDN_TO_LITR2N                     	long_name         .decomp. of coarse woody debris N to litter 2 N     units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   CWDN_TO_LITR3N                     	long_name         .decomp. of coarse woody debris N to litter 3 N     units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   CWDN_vr                       	long_name         CWD N (vertically resolved)    units         gN/m^3     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     �   CWDP                   	long_name         CWD P      units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �P   CWDP_TO_LITR2P                     	long_name         .decomp. of coarse woody debris P to litter 2 N     units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �T   CWDP_TO_LITR3P                     	long_name         .decomp. of coarse woody debris P to litter 3 N     units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �X   CWDP_vr                       	long_name         CWD P (vertically resolved)    units         gP/m^3     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     �\   
DEADCROOTC                     	long_name         dead coarse root C     units         gC/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   
DEADCROOTN                     	long_name         dead coarse root N     units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   
DEADCROOTP                     	long_name         dead coarse root P     units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   	DEADSTEMC                      	long_name         dead stem C    units         gC/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   	DEADSTEMN                      	long_name         dead stem N    units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   	DEADSTEMP                      	long_name         dead stem P    units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   DENIT                      	long_name         total rate of denitrification      units         gN/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   DISPVEGC                   	long_name         1displayed veg carbon, excluding storage and cpool      units         gC/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   DISPVEGN                   	long_name         displayed vegetation nitrogen      units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   DISPVEGP                   	long_name         displayed vegetation phosphorus    units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   DSTDEP                     	long_name         /total dust deposition (dry+wet) from atmosphere    units         kg/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   DSTFLXT                    	long_name         total surface dust emission    units         kg/m2/s    cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   	DWT_CLOSS                      	long_name         ,total carbon loss from land cover conversion   units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   DWT_CONV_CFLUX                     	long_name         )conversion C flux (immediate loss to atm)      units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   DWT_CONV_NFLUX                     	long_name         )conversion N flux (immediate loss to atm)      units         gN/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   DWT_CONV_PFLUX                     	long_name         )conversion P flux (immediate loss to atm)      units         gP/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   	DWT_NLOSS                      	long_name         -total nitrogen loss from landcover conversion      units         gN/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   	DWT_PLOSS                      	long_name         /total phosphorus loss from landcover conversion    units         gP/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   DWT_PROD100C_GAIN                      	long_name         <landcover change-driven addition to 100-yr wood product pool   units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   DWT_PROD100N_GAIN                      	long_name         $addition to 100-yr wood product pool   units         gN/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   DWT_PROD100P_GAIN                      	long_name         $addition to 100-yr wood product pool   units         gP/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   DWT_PROD10C_GAIN                   	long_name         ;landcover change-driven addition to 10-yr wood product pool    units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   DWT_PROD10N_GAIN                   	long_name         #addition to 10-yr wood product pool    units         gN/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   DWT_PROD10P_GAIN                   	long_name         #addition to 10-yr wood product pool    units         gP/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   DWT_SEEDC_TO_DEADSTEM                      	long_name         #seed source to patch-level deadstem    units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   DWT_SEEDC_TO_LEAF                      	long_name         seed source to patch-level leaf    units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   DWT_SEEDN_TO_DEADSTEM                      	long_name         !seed source to PFT-level deadstem      units         gN/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �    DWT_SEEDN_TO_LEAF                      	long_name         seed source to PFT-level leaf      units         gN/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   DWT_SEEDP_TO_DEADSTEM                      	long_name         !seed source to PFT-level deadstem      units         gP/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   DWT_SEEDP_TO_LEAF                      	long_name         seed source to PFT-level leaf      units         gP/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   EFLX_DYNBAL                    	long_name         0dynamic land cover change conversion energy flux   units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   EFLX_GRND_LAKE                     	long_name         Bnet heat flux into lake/snow surface, excluding light transmission     units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   EFLX_LH_TOT                    	long_name         !total latent heat flux [+ to atm]      units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   EFLX_LH_TOT_R                      	long_name         Rural total evaporation    units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   EFLX_LH_TOT_U                      	long_name         Urban total evaporation    units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �    ELAI                   	long_name         !exposed one-sided leaf area index      units         m^2/m^2    cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �$   ER                     	long_name         8total ecosystem respiration, autotrophic + heterotrophic   units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �(   ERRH2O                     	long_name         total water conservation error     units         mm     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �,   	ERRH2OSNO                      	long_name         &imbalance in snow depth (liquid water)     units         mm     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �0   ERRSEB                     	long_name         !surface energy conservation error      units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �4   ERRSOI                     	long_name         #soil/lake energy conservation error    units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �8   ERRSOL                     	long_name         "solar radiation conservation error     units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �<   ESAI                   	long_name         !exposed one-sided stem area index      units         m^2/m^2    cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �@   FAREA_BURNED                   	long_name         timestep fractional area burned    units         
proportion     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �D   FCEV                   	long_name         canopy evaporation     units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �H   FCOV                   	long_name         fractional impermeable area    units         unitless   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �L   FCTR                   	long_name         canopy transpiration   units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �P   FGEV                   	long_name         ground evaporation     units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �T   FGR                    	long_name         Oheat flux into soil/snow including snow melt and lake / snow light transmission    units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �X   FGR12                      	long_name         %heat flux between soil layers 1 and 2      units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �\   FGR_R                      	long_name         NRural heat flux into soil/snow including snow melt and snow light transmission     units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �`   FGR_U                      	long_name         2Urban heat flux into soil/snow including snow melt     units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �d   FH2OSFC                    	long_name         +fraction of ground covered by surface water    units         unitless   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �h   FIRA                   	long_name         !net infrared (longwave) radiation      units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �l   FIRA_R                     	long_name         'Rural net infrared (longwave) radiation    units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �p   FIRA_U                     	long_name         'Urban net infrared (longwave) radiation    units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �t   FIRE                   	long_name         %emitted infrared (longwave) radiation      units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �x   FIRE_R                     	long_name         +Rural emitted infrared (longwave) radiation    units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �|   FIRE_U                     	long_name         +Urban emitted infrared (longwave) radiation    units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   FLDS                   	long_name         atmospheric longwave radiation     units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   FPG                    	long_name         -fraction of potential gpp due to N limitation      units         
proportion     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   FPG_P                      	long_name         -fraction of potential gpp due to P limitation      units         
proportion     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   FPI                    	long_name         0fraction of potential immobilization of nitrogen   units         
proportion     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   FPI_P                      	long_name         2fraction of potential immobilization of phosphorus     units         
proportion     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   FPI_P_vr                      	long_name         2fraction of potential immobilization of phosphorus     units         
proportion     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     ��   FPI_vr                        	long_name         0fraction of potential immobilization of nitrogen   units         
proportion     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     ��   FPSN                   	long_name         photosynthesis     units         umol/m2s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   FPSN_WC                    	long_name         Rubisco-limited photosynthesis     units         umol/m2s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   FPSN_WJ                    	long_name         RuBP-limited photosynthesis    units         umol/m2s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   FPSN_WP                    	long_name         Product-limited photosynthesis     units         umol/m2s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   FROOTC                     	long_name         fine root C    units         gC/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �    FROOTC_ALLOC                   	long_name         fine root C allocation     units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �$   FROOTC_LOSS                    	long_name         fine root C loss   units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �(   FROOTN                     	long_name         fine root N    units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �,   FROOTP                     	long_name         fine root P    units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �0   FROST_TABLE                    	long_name         ,frost table depth (vegetated landunits only)   units         m      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �4   FSA                    	long_name         absorbed solar radiation   units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �8   FSAT                   	long_name         +fractional area with water table at surface    units         unitless   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �<   FSA_R                      	long_name         Rural absorbed solar radiation     units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �@   FSA_U                      	long_name         Urban absorbed solar radiation     units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �D   FSDS                   	long_name         $atmospheric incident solar radiation   units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �H   FSDSND                     	long_name         #direct nir incident solar radiation    units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �L   FSDSNDLN                   	long_name         1direct nir incident solar radiation at local noon      units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �P   FSDSNI                     	long_name         $diffuse nir incident solar radiation   units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �T   FSDSVD                     	long_name         #direct vis incident solar radiation    units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �X   FSDSVDLN                   	long_name         1direct vis incident solar radiation at local noon      units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �\   FSDSVI                     	long_name         $diffuse vis incident solar radiation   units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �`   FSDSVILN                   	long_name         2diffuse vis incident solar radiation at local noon     units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �d   FSH                    	long_name         sensible heat      units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �h   FSH_G                      	long_name         sensible heat from ground      units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �l   FSH_NODYNLNDUSE                    	long_name         :sensible heat not including correction for land use change     units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �p   FSH_R                      	long_name         Rural sensible heat    units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �t   FSH_U                      	long_name         Urban sensible heat    units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �x   FSH_V                      	long_name         sensible heat from veg     units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �|   FSM                    	long_name         snow melt heat flux    units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   FSM_R                      	long_name         Rural snow melt heat flux      units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   FSM_U                      	long_name         Urban snow melt heat flux      units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   FSNO                   	long_name         "fraction of ground covered by snow     units         unitless   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   FSNO_EFF                   	long_name         ,effective fraction of ground covered by snow   units         unitless   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   FSR                    	long_name         reflected solar radiation      units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   FSRND                      	long_name         $direct nir reflected solar radiation   units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   FSRNDLN                    	long_name         2direct nir reflected solar radiation at local noon     units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   FSRNI                      	long_name         %diffuse nir reflected solar radiation      units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   FSRVD                      	long_name         $direct vis reflected solar radiation   units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   FSRVDLN                    	long_name         2direct vis reflected solar radiation at local noon     units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   FSRVI                      	long_name         %diffuse vis reflected solar radiation      units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   FUELC                      	long_name         	fuel load      units         gC/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   
F_CO2_SOIL                     	long_name         total soil-atm. CO2 exchange   units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   F_CO2_SOIL_vr                         	long_name         0total vertically resolved soil-atm. CO2 exchange   units         gC/m^3/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     ��   GC_HEAT1                   	long_name         #initial gridcell total heat content    units         J/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   GC_ICE1                    	long_name         "initial gridcell total ice content     units         mm     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   GC_LIQ1                    	long_name         "initial gridcell total liq content     units         mm     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   GPP                    	long_name         gross primary production   units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �    GR                     	long_name         total growth respiration   units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   
GROSS_NMIN                     	long_name         gross rate of N mineralization     units         gN/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   
GROSS_PMIN                     	long_name         gross rate of P mineralization     units         gP/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   H2OCAN                     	long_name         intercepted water      units         mm     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   H2OSFC                     	long_name         surface water depth    units         mm     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   H2OSNO                     	long_name         snow depth (liquid water)      units         mm     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   
H2OSNO_TOP                     	long_name         mass of snow in top snow layer     units         kg/m2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   H2OSOI                        	long_name         0volumetric soil water (vegetated landunits only)   units         mm3/mm3    cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     �    HC                     	long_name         heat content of soil/snow/lake     units         MJ/m2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �\   HCSOI                      	long_name         soil heat content      units         MJ/m2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �`   HEAT_FROM_AC                   	long_name         Lsensible heat flux put into canyon due to heat removed from air conditioning   units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �d   HR                     	long_name         total heterotrophic respiration    units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �h   HR_vr                         	long_name         3total vertically resolved heterotrophic respiration    units         gC/m^3/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     �l   HTOP                   	long_name         
canopy top     units         m      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   INT_SNOW                   	long_name         *accumulated swe (vegetated landunits only)     units         mm     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   LABILEP                    	long_name         soil Labile P      units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   LABILEP_TO_SECONDP                     	long_name         LABILE P TO SECONDARY MINERAL P    units         gP/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   
LABILEP_vr                        	long_name         soil labile P (vert. res.)     units         gp/m^3     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     ��   LAISHA                     	long_name          shaded projected leaf area index   units         none   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   LAISUN                     	long_name          sunlit projected leaf area index   units         none   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   LAKEICEFRAC                       	long_name         lake layer ice mass fraction   units         unitless   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      (     ��   LAKEICETHICK                   	long_name         @thickness of lake ice (including physical expansion on freezing)   units         m      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �$   LAND_UPTAKE                    	long_name         ,NEE minus LAND_USE_FLUX, negative for update   units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �(   LAND_USE_FLUX                      	long_name         Atotal C emitted from land cover conversion and wood product pools      units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �,   LEAFC                      	long_name         leaf C     units         gC/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �0   LEAFC_ALLOC                    	long_name         leaf C allocation      units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �4   
LEAFC_LOSS                     	long_name         leaf C loss    units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �8   LEAFN                      	long_name         leaf N     units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �<   LEAFP                      	long_name         leaf P     units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �@   LEAF_MR                    	long_name         leaf maintenance respiration   units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �D   LFC2                   	long_name         3conversion area fraction of BET and BDT that burned    units         per sec    cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �H   LF_CONV_CFLUX                      	long_name         4conversion carbon due to BET and BDT area decreasing   units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �L   LITFALL                    	long_name         "litterfall (leaves and fine roots)     units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �P   LITHR                      	long_name          litter heterotrophic respiration   units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �T   LITR1C                     	long_name         LITR1 C    units         gC/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �X   LITR1C_TO_SOIL1C                   	long_name         !decomp. of litter 1 C to soil 1 C      units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �\   	LITR1C_vr                         	long_name         LITR1 C (vertically resolved)      units         gC/m^3     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     �`   LITR1N                     	long_name         LITR1 N    units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   LITR1N_TNDNCY_VERT_TRANS                      	long_name         -litter 1 N tendency due to vertical transport      units         gN/m^3/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     ��   LITR1N_TO_SOIL1N                   	long_name         !decomp. of litter 1 N to soil 1 N      units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   	LITR1N_vr                         	long_name         LITR1 N (vertically resolved)      units         gN/m^3     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     ��   LITR1P                     	long_name         LITR1 P    units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   LITR1P_TNDNCY_VERT_TRANS                      	long_name         -litter 1 P tendency due to vertical transport      units         gP/m^3/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     �    LITR1P_TO_SOIL1P                   	long_name         !decomp. of litter 1 P to soil 1 N      units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �\   	LITR1P_vr                         	long_name         LITR1 P (vertically resolved)      units         gP/m^3     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     �`   LITR1_HR                   	long_name         Het. Resp. from litter 1   units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   LITR2C                     	long_name         LITR2 C    units         gC/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   LITR2C_TO_SOIL2C                   	long_name         !decomp. of litter 2 C to soil 2 C      units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   	LITR2C_vr                         	long_name         LITR2 C (vertically resolved)      units         gC/m^3     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     ��   LITR2N                     	long_name         LITR2 N    units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   LITR2N_TNDNCY_VERT_TRANS                      	long_name         -litter 2 N tendency due to vertical transport      units         gN/m^3/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     ��   LITR2N_TO_SOIL2N                   	long_name         !decomp. of litter 2 N to soil 2 N      units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �$   	LITR2N_vr                         	long_name         LITR2 N (vertically resolved)      units         gN/m^3     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     �(   LITR2P                     	long_name         LITR2 P    units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �d   LITR2P_TNDNCY_VERT_TRANS                      	long_name         -litter 2 P tendency due to vertical transport      units         gP/m^3/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     �h   LITR2P_TO_SOIL2P                   	long_name         !decomp. of litter 2 P to soil 2 N      units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   	LITR2P_vr                         	long_name         LITR2 P (vertically resolved)      units         gP/m^3     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     ��   LITR2_HR                   	long_name         Het. Resp. from litter 2   units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   LITR3C                     	long_name         LITR3 C    units         gC/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   LITR3C_TO_SOIL3C                   	long_name         !decomp. of litter 3 C to soil 3 C      units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   	LITR3C_vr                         	long_name         LITR3 C (vertically resolved)      units         gC/m^3     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     ��   LITR3N                     	long_name         LITR3 N    units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �,   LITR3N_TNDNCY_VERT_TRANS                      	long_name         -litter 3 N tendency due to vertical transport      units         gN/m^3/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     �0   LITR3N_TO_SOIL3N                   	long_name         !decomp. of litter 3 N to soil 3 N      units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �l   	LITR3N_vr                         	long_name         LITR3 N (vertically resolved)      units         gN/m^3     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     �p   LITR3P                     	long_name         LITR3 P    units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   LITR3P_TNDNCY_VERT_TRANS                      	long_name         -litter 3 P tendency due to vertical transport      units         gP/m^3/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     ��   LITR3P_TO_SOIL3P                   	long_name         !decomp. of litter 3 P to soil 3 N      units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   	LITR3P_vr                         	long_name         LITR3 P (vertically resolved)      units         gP/m^3     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     ��   LITR3_HR                   	long_name         Het. Resp. from litter 3   units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �,   LITTERC                    	long_name         litter C   units         gC/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �0   
LITTERC_HR                     	long_name         "litter C heterotrophic respiration     units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �4   LITTERC_LOSS                   	long_name         litter C loss      units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �8   
LIVECROOTC                     	long_name         live coarse root C     units         gC/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �<   
LIVECROOTN                     	long_name         live coarse root N     units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �@   
LIVECROOTP                     	long_name         live coarse root P     units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �D   	LIVESTEMC                      	long_name         live stem C    units         gC/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �H   	LIVESTEMN                      	long_name         live stem N    units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �L   	LIVESTEMP                      	long_name         live stem P    units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �P   MEG_acetaldehyde                   	long_name         
MEGAN flux     units         	kg/m2/sec      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �T   MEG_acetic_acid                    	long_name         
MEGAN flux     units         	kg/m2/sec      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �X   MEG_acetone                    	long_name         
MEGAN flux     units         	kg/m2/sec      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �\   MEG_carene_3                   	long_name         
MEGAN flux     units         	kg/m2/sec      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �`   MEG_ethanol                    	long_name         
MEGAN flux     units         	kg/m2/sec      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �d   MEG_formaldehyde                   	long_name         
MEGAN flux     units         	kg/m2/sec      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �h   MEG_isoprene                   	long_name         
MEGAN flux     units         	kg/m2/sec      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �l   MEG_methanol                   	long_name         
MEGAN flux     units         	kg/m2/sec      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �p   MEG_pinene_a                   	long_name         
MEGAN flux     units         	kg/m2/sec      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �t   MEG_thujene_a                      	long_name         
MEGAN flux     units         	kg/m2/sec      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �x   MR                     	long_name         maintenance respiration    units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �|   M_LITR1C_TO_LEACHING                   	long_name         litter 1 C leaching loss   units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   M_LITR2C_TO_LEACHING                   	long_name         litter 2 C leaching loss   units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   M_LITR3C_TO_LEACHING                   	long_name         litter 3 C leaching loss   units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   M_SOIL1C_TO_LEACHING                   	long_name         soil 1 C leaching loss     units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   M_SOIL2C_TO_LEACHING                   	long_name         soil 2 C leaching loss     units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   M_SOIL3C_TO_LEACHING                   	long_name         soil 3 C leaching loss     units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   M_SOIL4C_TO_LEACHING                   	long_name         soil 4 C leaching loss     units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   NBP                    	long_name         Qnet biome production, includes fire, landuse, and harvest flux, positive for sink      units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   NDEPLOY                    	long_name         total N deployed in new growth     units         gN/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   NDEP_TO_SMINN                      	long_name         *atmospheric N deposition to soil mineral N     units         gN/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   NEE                    	long_name         mnet ecosystem exchange of carbon, includes fire, landuse, harvest, and hrv_xsmrpool flux, positive for source      units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   NEP                    	long_name         Unet ecosystem production, excludes fire, landuse, and harvest flux, positive for sink      units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   NET_NMIN                   	long_name         net rate of N mineralization   units         gN/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   NET_PMIN                   	long_name         net rate of P mineralization   units         gP/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   NFIRE                      	long_name         fire counts valid only in Reg.C    units         counts/km2/sec     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   NFIX_TO_SMINN                      	long_name         1symbiotic/asymbiotic N fixation to soil mineral N      units         gN/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   NPP                    	long_name         net primary production     units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   OCCLP                      	long_name         soil occluded P    units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ��   OCCLP_vr                      	long_name         soil occluded P (vert. res.)   units         gp/m^3     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     ��   OCDEP                      	long_name         -total OC deposition (dry+wet) from atmosphere      units         kg/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��               O_SCALAR                      	long_name         8fraction by which decomposition is reduced due to anoxia   units         unitless   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <         PARVEGLN                   	long_name         (absorbed par by vegetation at local noon   units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��            D   PBOT                   	long_name         atmospheric pressure   units         Pa     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��            H   PCO2                   	long_name         #atmospheric partial pressure of CO2    units         Pa     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��            L   PCT_LANDUNIT         
             	long_name         % of each landunit on grid cell    units         %      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      $      P   PCT_NAT_PFT                       	long_name         =% of each PFT on the natural vegetation (i.e., soil) landunit      units         %      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      D      t   PDEPLOY                    	long_name         total P deployed in new growth     units         gP/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��            �   PDEP_TO_SMINP                      	long_name         *atmospheric P deposition to soil mineral P     units         gP/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��            �   
PFT_CTRUNC                     	long_name         !patch-level sink for C truncation      units         gC/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��            �   PFT_FIRE_CLOSS                     	long_name         Stotal patch-level fire C loss for non-peat fires outside land-type converted region    units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��            �   PFT_FIRE_NLOSS                     	long_name         total pft-level fire N loss    units         gN/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��            �   PFT_FIRE_PLOSS                     	long_name         total pft-level fire P loss    units         gP/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��            �   
PFT_NTRUNC                     	long_name         pft-level sink for N truncation    units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��            �   
PFT_PTRUNC                     	long_name         pft-level sink for P truncation    units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��            �   PLANT_NDEMAND                      	long_name         &N flux required to support initial GPP     units         gN/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��            �   PLANT_NDEMAND_COL                      	long_name         &N flux required to support initial GPP     units         gN/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��            �   PLANT_PDEMAND                      	long_name         &P flux required to support initial GPP     units         gP/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��            �   PLANT_PDEMAND_COL                      	long_name         &P flux required to support initial GPP     units         gN/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��            �   POTENTIAL_IMMOB                    	long_name         potential N immobilization     units         gN/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��            �   POTENTIAL_IMMOB_P                      	long_name         potential P immobilization     units         gP/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��            �   PRIMP                      	long_name         soil primary P     units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��            �   PRIMP_TO_LABILEP                   	long_name         PRIMARY MINERAL P TO LABILE P      units         gP/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��            �   PRIMP_vr                      	long_name         soil primary P (vert. res.)    units         gp/m^3     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <      �   PROD100C                   	long_name         100-yr wood product C      units         gC/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           4   PROD100C_LOSS                      	long_name         "loss from 100-yr wood product pool     units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           8   PROD100N                   	long_name         100-yr wood product N      units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           <   PROD100N_LOSS                      	long_name         "loss from 100-yr wood product pool     units         gN/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           @   PROD100P                   	long_name         100-yr wood product P      units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           D   PROD100P_LOSS                      	long_name         "loss from 100-yr wood product pool     units         gP/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           H   PROD10C                    	long_name         10-yr wood product C   units         gC/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           L   PROD10C_LOSS                   	long_name         !loss from 10-yr wood product pool      units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           P   PROD10N                    	long_name         10-yr wood product N   units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           T   PROD10N_LOSS                   	long_name         !loss from 10-yr wood product pool      units         gN/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           X   PROD10P                    	long_name         10-yr wood product P   units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           \   PROD10P_LOSS                   	long_name         !loss from 10-yr wood product pool      units         gP/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           `   PROD1C                     	long_name         1-yr crop product C    units         gC/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           d   PROD1C_LOSS                    	long_name          loss from 1-yr crop product pool   units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           h   PROD1N                     	long_name         1-yr crop product N    units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           l   PROD1N_LOSS                    	long_name          loss from 1-yr crop product pool   units         gN/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           p   PRODUCT_CLOSS                      	long_name         )total carbon loss from wood product pools      units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           t   PRODUCT_NLOSS                      	long_name         $total N loss from wood product pools   units         gN/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           x   PRODUCT_PLOSS                      	long_name         $total P loss from wood product pools   units         gP/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           |   PSNSHA                     	long_name         shaded leaf photosynthesis     units         umolCO2/m^2/s      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   PSNSHADE_TO_CPOOL                      	long_name         C fixation from shaded canopy      units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   PSNSUN                     	long_name         sunlit leaf photosynthesis     units         umolCO2/m^2/s      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   PSNSUN_TO_CPOOL                    	long_name         C fixation from sunlit canopy      units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   Q2M                    	long_name         2m specific humidity   units         kg/kg      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   QBOT                   	long_name         atmospheric specific humidity      units         kg/kg      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   QCHARGE                    	long_name         0aquifer recharge rate (vegetated landunits only)   units         mm/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   QDRAI                      	long_name         sub-surface drainage   units         mm/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   QDRAI_PERCH                    	long_name         perched wt drainage    units         mm/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   QDRAI_XS                   	long_name         saturation excess drainage     units         mm/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   QDRIP                      	long_name         throughfall    units         mm/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   QFLOOD                     	long_name         runoff from river flooding     units         mm/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   QFLX_ICE_DYNBAL                    	long_name         4ice dynamic land cover change conversion runoff flux   units         mm/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   QFLX_LIQ_DYNBAL                    	long_name         4liq dynamic land cover change conversion runoff flux   units         mm/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   QH2OSFC                    	long_name         surface water runoff   units         mm/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   QINFL                      	long_name         infiltration   units         mm/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   QINTR                      	long_name         interception   units         mm/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   QIRRIG                     	long_name         water added through irrigation     units         mm/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   QOVER                      	long_name         surface runoff     units         mm/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   QRGWL                      	long_name         9surface runoff at glaciers (liquid only), wetlands, lakes      units         mm/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   QRUNOFF                    	long_name         0total liquid runoff (does not include QSNWCPICE)   units         mm/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   QRUNOFF_NODYNLNDUSE                    	long_name         ]total liquid runoff (does not include QSNWCPICE) not including correction for land use change      units         mm/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   	QRUNOFF_R                      	long_name         Rural total runoff     units         mm/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   	QRUNOFF_U                      	long_name         Urban total runoff     units         mm/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   QSNOMELT                   	long_name         	snow melt      units         mm/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   	QSNWCPICE                      	long_name         #excess snowfall due to snow capping    units         mm/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   QSNWCPICE_NODYNLNDUSE                      	long_name         Pexcess snowfall due to snow capping not including correction for land use change   units         mm H2O/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   QSOIL                      	long_name         HGround evaporation (soil/snow evaporation + soil/snow sublimation - dew)   units         mm/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   QVEGE                      	long_name         canopy evaporation     units         mm/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   QVEGT                      	long_name         canopy transpiration   units         mm/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   RAIN                   	long_name         atmospheric rain   units         mm/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   RETRANSN                   	long_name         plant pool of retranslocated N     units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   RETRANSN_TO_NPOOL                      	long_name         deployment of retranslocated N     units         gN/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��               RETRANSP                   	long_name         plant pool of retranslocated P     units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��              RETRANSP_TO_PPOOL                      	long_name         deployment of retranslocated P     units         gP/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��              RH2M                   	long_name         2m relative humidity   units         %      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��              RH2M_R                     	long_name         Rural 2m specific humidity     units         %      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��              RH2M_U                     	long_name         Urban 2m relative humidity     units         %      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��              RR                     	long_name         /root respiration (fine root MR + total root GR)    units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��              RSCANOPY                   	long_name         canopy resistance      units          s m-1     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��              SABG                   	long_name         solar rad absorbed by ground   units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��               SABG_PEN                   	long_name         2Rural solar rad penetrating top soil or snow layer     units         watt/m^2   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           $   SABV                   	long_name         solar rad absorbed by veg      units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           (   	SCALARAVG                      	long_name         average of decomposition scalar    units         fraction   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ,   SECONDP                    	long_name         soil secondary P   units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           0   SECONDP_TO_LABILEP                     	long_name         SECONDARY MINERAL P TO LABILE P    units         gP/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           4   SECONDP_TO_OCCLP                   	long_name         !SECONDARY MINERAL P TO OCCLUDED P      units         gP/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           8   
SECONDP_vr                        	long_name         soil secondary P (vert. res.)      units         gp/m^3     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     <   SEEDC                      	long_name         pool for seeding new Patches   units         gC/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           x   SEEDN                      	long_name         pool for seeding new PFTs      units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           |   SEEDP                      	long_name         P pool for seeding new PFTs    units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   SMINN                      	long_name         soil mineral N     units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   SMINN_LEACHED                      	long_name         $soil mineral N pool loss to leaching   units         gN/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   SMINN_TO_DENIT_L1S1                    	long_name         /denitrification for decomp. of litter 1to SOIL1    units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   SMINN_TO_DENIT_L2S2                    	long_name         /denitrification for decomp. of litter 2to SOIL2    units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   SMINN_TO_DENIT_L3S3                    	long_name         /denitrification for decomp. of litter 3to SOIL3    units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   SMINN_TO_DENIT_S1S2                    	long_name         -denitrification for decomp. of soil 1to SOIL2      units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   SMINN_TO_DENIT_S2S3                    	long_name         -denitrification for decomp. of soil 2to SOIL3      units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   SMINN_TO_DENIT_S3S4                    	long_name         -denitrification for decomp. of soil 3to SOIL4      units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   SMINN_TO_DENIT_S4                      	long_name         0denitrification for decomp. of soil 4to atmosphe   units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   SMINN_TO_NPOOL                     	long_name         #deployment of soil mineral N uptake    units         gN/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   SMINN_TO_PLANT                     	long_name         plant uptake of soil mineral N     units         gN/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   SMINN_TO_SOIL1N_L1                     	long_name         +mineral N flux for decomp. of LITR1to SOIL1    units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   SMINN_TO_SOIL2N_L2                     	long_name         +mineral N flux for decomp. of LITR2to SOIL2    units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   SMINN_TO_SOIL2N_S1                     	long_name         +mineral N flux for decomp. of SOIL1to SOIL2    units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   SMINN_TO_SOIL3N_L3                     	long_name         +mineral N flux for decomp. of LITR3to SOIL3    units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   SMINN_TO_SOIL3N_S2                     	long_name         +mineral N flux for decomp. of SOIL2to SOIL3    units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   SMINN_TO_SOIL4N_S3                     	long_name         +mineral N flux for decomp. of SOIL3to SOIL4    units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   SMINN_vr                      	long_name         soil mineral N     units         gN/m^3     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     �   SMINP                      	long_name         soil mineral P     units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��              SMINP_LEACHED                      	long_name         $soil mineral P pool loss to leaching   units         gP/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��              SMINP_TO_PLANT                     	long_name         plant uptake of soil mineral P     units         gP/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��              SMINP_TO_SOIL1P_L1                     	long_name         +mineral P flux for decomp. of LITR1to SOIL1    units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��              SMINP_TO_SOIL2P_L2                     	long_name         +mineral P flux for decomp. of LITR2to SOIL2    units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��              SMINP_TO_SOIL2P_S1                     	long_name         +mineral P flux for decomp. of SOIL1to SOIL2    units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��              SMINP_TO_SOIL3P_L3                     	long_name         +mineral P flux for decomp. of LITR3to SOIL3    units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��              SMINP_TO_SOIL3P_S2                     	long_name         +mineral P flux for decomp. of SOIL2to SOIL3    units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��               SMINP_TO_SOIL4P_S3                     	long_name         +mineral P flux for decomp. of SOIL3to SOIL4    units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           $   SMINP_vr                      	long_name         soil mineral P (vert. res.)    units         gp/m^3     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     (   SMIPP_TO_NPOOL                     	long_name         #deployment of soil mineral P uptake    units         gP/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           d   SNOBCMCL                   	long_name         mass of BC in snow column      units         kg/m2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           h   SNOBCMSL                   	long_name         mass of BC in top snow layer   units         kg/m2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           l   	SNODSTMCL                      	long_name         mass of dust in snow column    units         kg/m2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           p   	SNODSTMSL                      	long_name         mass of dust in top snow layer     units         kg/m2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           t   	SNOINTABS                      	long_name         7Percent of incoming solar absorbed by lower snow layers    units         %      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           x   SNOOCMCL                   	long_name         mass of OC in snow column      units         kg/m2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           |   SNOOCMSL                   	long_name         mass of OC in top snow layer   units         kg/m2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   SNOW                   	long_name         atmospheric snow   units         mm/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   SNOWDP                     	long_name         gridcell mean snow height      units         m      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   SNOWICE                    	long_name         snow ice   units         kg/m2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   SNOWLIQ                    	long_name         snow liquid water      units         kg/m2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   
SNOW_DEPTH                     	long_name          snow height of snow covered area   units         m      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   
SNOW_SINKS                     	long_name         snow sinks (liquid water)      units         mm/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   SNOW_SOURCES                   	long_name         snow sources (liquid water)    units         mm/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   SOIL1C                     	long_name         SOIL1 C    units         gC/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   SOIL1C_TO_SOIL2C                   	long_name         decomp. of soil 1 C to soil 2 C    units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   	SOIL1C_vr                         	long_name         SOIL1 C (vertically resolved)      units         gC/m^3     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     �   SOIL1N                     	long_name         SOIL1 N    units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   SOIL1N_TNDNCY_VERT_TRANS                      	long_name         +soil 1 N tendency due to vertical transport    units         gN/m^3/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     �   SOIL1N_TO_SOIL2N                   	long_name         decomp. of soil 1 N to soil 2 N    units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           $   	SOIL1N_vr                         	long_name         SOIL1 N (vertically resolved)      units         gN/m^3     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     (   SOIL1P                     	long_name         SOIL1 P    units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           d   SOIL1P_TNDNCY_VERT_TRANS                      	long_name         +soil 1 P tendency due to vertical transport    units         gP/m^3/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     h   SOIL1P_TO_SOIL2P                   	long_name         decomp. of soil 1 P to soil 2 N    units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   	SOIL1P_vr                         	long_name         SOIL1 P (vertically resolved)      units         gP/m^3     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     �   SOIL1_HR                   	long_name         Het. Resp. from soil 1     units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   SOIL2C                     	long_name         SOIL2 C    units         gC/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   SOIL2C_TO_SOIL3C                   	long_name         decomp. of soil 2 C to soil 3 C    units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   	SOIL2C_vr                         	long_name         SOIL2 C (vertically resolved)      units         gC/m^3     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     �   SOIL2N                     	long_name         SOIL2 N    units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ,   SOIL2N_TNDNCY_VERT_TRANS                      	long_name         +soil 2 N tendency due to vertical transport    units         gN/m^3/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     0   SOIL2N_TO_SOIL3N                   	long_name         decomp. of soil 2 N to soil 3 N    units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           l   	SOIL2N_vr                         	long_name         SOIL2 N (vertically resolved)      units         gN/m^3     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     p   SOIL2P                     	long_name         SOIL2 P    units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   SOIL2P_TNDNCY_VERT_TRANS                      	long_name         +soil 2 P tendency due to vertical transport    units         gP/m^3/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     �   SOIL2P_TO_SOIL3P                   	long_name         decomp. of soil 2 P to soil 3 N    units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   	SOIL2P_vr                         	long_name         SOIL2 P (vertically resolved)      units         gP/m^3     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     �   SOIL2_HR                   	long_name         Het. Resp. from soil 2     units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ,   SOIL3C                     	long_name         SOIL3 C    units         gC/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           0   SOIL3C_TO_SOIL4C                   	long_name         decomp. of soil 3 C to soil 4 C    units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           4   	SOIL3C_vr                         	long_name         SOIL3 C (vertically resolved)      units         gC/m^3     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     8   SOIL3N                     	long_name         SOIL3 N    units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           t   SOIL3N_TNDNCY_VERT_TRANS                      	long_name         +soil 3 N tendency due to vertical transport    units         gN/m^3/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     x   SOIL3N_TO_SOIL4N                   	long_name         decomp. of soil 3 N to soil 4 N    units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   	SOIL3N_vr                         	long_name         SOIL3 N (vertically resolved)      units         gN/m^3     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     �   SOIL3P                     	long_name         SOIL3 P    units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   SOIL3P_TNDNCY_VERT_TRANS                      	long_name         +soil 3 P tendency due to vertical transport    units         gP/m^3/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     �   SOIL3P_TO_SOIL4P                   	long_name         decomp. of soil 3 P to soil 4 N    units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           4   	SOIL3P_vr                         	long_name         SOIL3 P (vertically resolved)      units         gP/m^3     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     8   SOIL3_HR                   	long_name         Het. Resp. from soil 3     units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           t   SOIL4C                     	long_name         SOIL4 C    units         gC/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           x   	SOIL4C_vr                         	long_name         SOIL4 C (vertically resolved)      units         gC/m^3     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     |   SOIL4N                     	long_name         SOIL4 N    units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   SOIL4N_TNDNCY_VERT_TRANS                      	long_name         +soil 4 N tendency due to vertical transport    units         gN/m^3/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     �   SOIL4N_TO_SMINN                    	long_name         #mineral N flux for decomp. of SOIL4    units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   	SOIL4N_vr                         	long_name         SOIL4 N (vertically resolved)      units         gN/m^3     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     �   SOIL4P                     	long_name         SOIL4 P    units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           8   SOIL4P_TNDNCY_VERT_TRANS                      	long_name         +soil 4 P tendency due to vertical transport    units         gP/m^3/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     <   SOIL4P_TO_SMINP                    	long_name         #mineral P flux for decomp. of SOIL4    units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           x   	SOIL4P_vr                         	long_name         SOIL4 P (vertically resolved)      units         gP/m^3     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     |   SOIL4_HR                   	long_name         Het. Resp. from soil 4     units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   SOILC                      	long_name         soil C     units         gC/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   SOILC_HR                   	long_name          soil C heterotrophic respiration   units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   
SOILC_LOSS                     	long_name         soil C loss    units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   SOILICE                       	long_name         #soil ice (vegetated landunits only)    units         kg/m2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     �   SOILLIQ                       	long_name         ,soil liquid water (vegetated landunits only)   units         kg/m2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     	   SOILPSI                       	long_name         'soil water potential in each soil layer    units         MPa    cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     	@   SOILWATER_10CM                     	long_name         @soil liquid water + ice in top 10cm of soil (veg landunits only)   units         kg/m2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           	|   	SOLUTIONP                      	long_name         soil solution P    units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           	�   SOLUTIONP_vr                      	long_name         soil solution P (vert. res.)   units         gp/m^3     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     	�   	SOMC_FIRE                      	long_name         C loss due to peat burning     units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           	�   SOMHR                      	long_name         -soil organic matter heterotrophic respiration      units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           	�   SOM_C_LEACHED                      	long_name         .total flux of C from SOM pools due to leaching     units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           	�   SR                     	long_name         'total soil respiration (HR + root resp)    units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           	�   STORVEGC                   	long_name         )stored vegetation carbon, excluding cpool      units         gC/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           	�   STORVEGN                   	long_name         stored vegetation nitrogen     units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           	�   STORVEGP                   	long_name         stored vegetation phosphorus   units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           	�   SUPPLEMENT_TO_SMINN                    	long_name         supplemental N supply      units         gN/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           	�   SUPPLEMENT_TO_SMINP                    	long_name         supplemental P supply      units         gP/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           	�   	SoilAlpha                      	long_name         factor limiting ground evap    units         unitless   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           	�   SoilAlpha_U                    	long_name         !urban factor limiting ground evap      units         unitless   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           	�   TAUX                   	long_name         zonal surface stress   units         kg/m/s^2   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           	�   TAUY                   	long_name         meridional surface stress      units         kg/m/s^2   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           	�   TBOT                   	long_name         atmospheric air temperature    units         K      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           	�   TBUILD                     	long_name         #internal urban building temperature    units         K      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           	�   TG                     	long_name         ground temperature     units         K      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           	�   TG_R                   	long_name         Rural ground temperature   units         K      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
    TG_U                   	long_name         Urban ground temperature   units         K      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
   TH2OSFC                    	long_name         surface water temperature      units         K      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
   THBOT                      	long_name         %atmospheric air potential temperature      units         K      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
   TKE1                   	long_name         (top lake level eddy thermal conductivity   units         W/(mK)     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
   TLAI                   	long_name         total projected leaf area index    units         none   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
   TLAKE                         	long_name         lake temperature   units         K      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      (     
   TOTCOLC                    	long_name         'total column carbon, incl veg and cpool    units         gC/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
@   TOTCOLN                    	long_name         total column-level N   units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
D   TOTCOLP                    	long_name         total column-level P   units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
H   
TOTECOSYSC                     	long_name         /total ecosystem carbon, incl veg but excl cpool    units         gC/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
L   
TOTECOSYSN                     	long_name         total ecosystem N      units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
P   
TOTECOSYSP                     	long_name         total ecosystem P      units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
T   TOTLITC                    	long_name         total litter carbon    units         gC/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
X   
TOTLITC_1m                     	long_name         $total litter carbon to 1 meter depth   units         gC/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
\   TOTLITN                    	long_name         total litter N     units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
`   
TOTLITN_1m                     	long_name         total litter N to 1 meter      units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
d   TOTLITP                    	long_name         total litter P     units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
h   
TOTLITP_1m                     	long_name         total litter P to 1 meter      units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
l   TOTPFTC                    	long_name         )total patch-level carbon, including cpool      units         gC/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
p   TOTPFTN                    	long_name         total PFT-level nitrogen   units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
t   TOTPFTP                    	long_name         total PFT-level phosphorus     units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
x   TOTPRODC                   	long_name         total wood product C   units         gC/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
|   TOTPRODN                   	long_name         total wood product N   units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
�   TOTPRODP                   	long_name         total wood product P   units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
�   TOTSOMC                    	long_name          total soil organic matter carbon   units         gC/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
�   
TOTSOMC_1m                     	long_name         1total soil organic matter carbon to 1 meter depth      units         gC/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
�   TOTSOMN                    	long_name         total soil organic matter N    units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
�   
TOTSOMN_1m                     	long_name         &total soil organic matter N to 1 meter     units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
�   TOTSOMP                    	long_name         total soil organic matter P    units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
�   
TOTSOMP_1m                     	long_name         &total soil organic matter P to 1 meter     units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
�   TOTVEGC                    	long_name         (total vegetation carbon, excluding cpool   units         gC/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
�   TOTVEGN                    	long_name         total vegetation nitrogen      units         gN/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
�   TOTVEGP                    	long_name         total vegetation phosphorus    units         gP/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
�   TREFMNAV                   	long_name         (daily minimum of average 2-m temperature   units         K      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
�   
TREFMNAV_R                     	long_name         .Rural daily minimum of average 2-m temperature     units         K      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
�   
TREFMNAV_U                     	long_name         .Urban daily minimum of average 2-m temperature     units         K      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
�   TREFMXAV                   	long_name         (daily maximum of average 2-m temperature   units         K      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
�   
TREFMXAV_R                     	long_name         .Rural daily maximum of average 2-m temperature     units         K      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
�   
TREFMXAV_U                     	long_name         .Urban daily maximum of average 2-m temperature     units         K      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
�   TSA                    	long_name         2m air temperature     units         K      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
�   TSAI                   	long_name         total projected stem area index    units         none   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
�   TSA_R                      	long_name         Rural 2m air temperature   units         K      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
�   TSA_U                      	long_name         Urban 2m air temperature   units         K      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           
�   TSOI                      	long_name         +soil temperature (vegetated landunits only)    units         K      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     
�   	TSOI_10CM                      	long_name         $soil temperature in top 10cm of soil   units         K      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��              TSOI_ICE                      	long_name         %soil temperature (ice landunits only)      units         K      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <        TV                     	long_name         vegetation temperature     units         K      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           P   TWS                    	long_name         total water storage    units         mm     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           T   T_SCALAR                      	long_name         'temperature inhibition of decomposition    units         unitless   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     X   U10                    	long_name         	10-m wind      units         m/s    cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   URBAN_AC                   	long_name         urban air conditioning flux    units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   
URBAN_HEAT                     	long_name         urban heating flux     units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   VOCFLXT                    	long_name         total VOC flux into atmosphere     units         moles/m2/sec   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   VOLR                   	long_name         river channel water storage    units         m3     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   WA                     	long_name         :water in the unconfined aquifer (vegetated landunits only)     units         mm     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   	WASTEHEAT                      	long_name         Csensible heat flux from heating/cooling sources of urban waste heat    units         W/m^2      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   WF                     	long_name         )soil water as frac. of whc for top 0.05 m      units         
proportion     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   WIND                   	long_name         #atmospheric wind velocity magnitude    units         m/s    cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   WOODC                      	long_name         wood C     units         gC/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   WOODC_ALLOC                    	long_name         wood C eallocation     units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   
WOODC_LOSS                     	long_name         wood C loss    units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   WOOD_HARVESTC                      	long_name         &wood harvest carbon (to product pools)     units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   WOOD_HARVESTN                      	long_name         !wood harvest N (to product pools)      units         gN/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   WOOD_HARVESTP                      	long_name         !wood harvest P (to product pools)      units         gP/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           �   W_SCALAR                      	long_name         .Moisture (dryness) inhibition of decomposition     units         unitless   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��      <     �   XSMRPOOL                   	long_name         temporary photosynthate C pool     units         gC/m^2     cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��              XSMRPOOL_RECOVER                   	long_name         0C flux assigned to recovery of negative xsmrpool   units         gC/m^2/s   cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��              ZBOT                   	long_name         atmospheric reference height   units         m      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��              ZWT                    	long_name         ,water table depth (vegetated landunits only)   units         m      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��              	ZWT_PERCH                      	long_name         4perched water table depth (vegetated landunits only)   units         m      cell_methods      
time: mean     
_FillValue        {@��   missing_value         {@��           ;�r<���=�=�o�>YI:>�l�?�~?��?�#'@7U�@��,@��rAN�A���B��=L��?��@ff@�33A��AI��A���A���B	L�B3�;�r<���=�=�o�>YI:>�l�?�~?��?�#'@7U�@��,@��rAN�A���B��©mwB6=<B�M�    ?�        GkA 3wU      �A     ,0@�h����@�h     05/09/1702:21:00                                    +j�.                9���            7�њ5��    @ѩ"                A��B��yB(|TAf��@]��>���                                    <V�I        =]>>Hu9=���<�+�;�X:l�                                    ; �        ;�Ѩ=�{<f
;�rj:�`;9��                                                                            .��9                                                                                {@��@�޹@�޹{@��        ��v��+Y@'�  /.*�                =?�'    @�޹���\�a����\{@��    B�jB�j{@��Ctx�Ctx�{@��CT&?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�                      ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�                                                          ;�r    =?�'    {@��        {@��        {@��    {@����������{@��            {@��?�  ?�          {@��        {@��    @�Z                                                                Fk�D��EZ4                9��z    Dz%@(�?6Op>X�>[�>F<�>�>�>'f>7(>�>�+7F�6�`{6O�5�/#5ʒFk�Fb�f                                                                    <#�
C�  ?mi�0�w>~�k>y˹>y��>yϊ>y�>y>y>y>y>y                            {@��{@��{@��{@��{@��{@��{@��{@��{@��{@��{@��                                        '���    =��X    <C�?��?1E=[s4=$��;�1�                                    ;�l                        	FW�                                    9�O�=C4<� a:�[�:�RZ9<P�                                    95B�                        �p�                                    7�;E��:�9ϫ8�ɺ7{|�                                        <RU    ?<�:9�*9�+ 9��                                            7�                ��                                            :��g5�*'5Pd5hw�                                            74"�                hK�                                            :�5SKH4�)5%�                                                <P    ?��9��R9�#92�                                            7��                $��                                            :�O�5)J4���4�?9                                            72��                ��                                            :<'4�o45��4K��                                                =�6�                                                                            ]	��0	"�� 	O߽��nO�                                        =��<3<<P<�<��<�<�<�<�<�                    +�ƿ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  ?�  {@��{@��{@��{@��{@��{@��G��B��B�                                                              B�                                              2+�            -��,j�d                        �1�0%�/�;36�;36�;36�;36�;36�;36�;36�;36�;36�;36                                                                                                                :	p�    ��ܧ*�o�                                                *�o�*�o�*�o�{@��            6�O                            B��B��{@��    {@��            <#�
A"�W0m],��&@0�8@+
9@+@+�@+ _@*��@*��@*��@*��@*��                                                                                                                                                                A1e8!L�                            @@��@:�?@:�@:��@:��@:�C@:�C@:�C@:�C@:�C                        :�[ 3��=�(36���    ;5h�4MT�    @��Dz%    @��6�O    >(ش    A��=H7�<�.$=��k:�0�7�q[                                    <a �                        ���                                    ??e;�ze;*�;�/99u�5�A�                                    9�##                        �f<                                    <�'}9`l8�v%99��6��35                                         >}S    A_�B<��<2�d<i�]                                            <���                &�                                            ?�D�:�=:n,�:���                                            :3��                	�~�                                            =8\8b=7�[8&:�                                                ?�    Bqq�=��=�t�=���                                            =�Y�                ��a                                            @�'�<CC<��<�k                                            ;b]                
���                                            =�<�9y�w9-��9AD`                                                E��2C�p�F��RF���F�� F��2E�zDCP8A���                            D7��                                ��                            BZcE�BEE	c3DߨOD7��B�@-@s                            Akh�                                ��U                            ?CB>k�B;	B/�"B$
Ak?� :=&��                                E��        A6ָ@m�@�CATɹAn�|A�4B/��B��CB�{C-n�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�@��5A9յA�gs<#�
<#�
<#�
<#�
<#�
�p  �p  �p  �p  �p  �p  �p  �p  �:��2E��p  �p  �p  �p  �p  A��/8�z@7��7��W7��:7�ݞ7��.7���7���7���7���7���                            U�-    7%X4�A�2��        ?�  {@�μ#S�    C�+�{@��C�B�C�B�{@��C��FC�+�{@��    {@��{@��{@��{@��{@��{@��{@��{@��{@��{@��E�%'D7��A�r�E�%&D7��Ak��=�6�=�6�;
@�;
@�9K��9K��7PK4�D�2�V            E��E��D7��D7� AkvAk6Z7%X4�A�2��C�5�C�5�{@��C�cC�c{@��C�;?   C�;{@��C��FC���C���C��+C���C��!C��C��C�UyC���C���C��%C��&C��"C���C��4{@��{@��{@��{@��{@��{@��{@��{@��{@��{@��{@��{@��{@��{@��{@��C�+�E��H>���>�>�m8>��)>�G>��>�]q>�8R>���>�>�{@��{@��{@��{@��{@��@%��                E}F    ?1�@�(�                                                                {@��{@��{@��{@��{@��        A�  A
A�;�r