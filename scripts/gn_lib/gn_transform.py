'''Helmert inversion and transformation functions'''
import numpy as _np
from .gn_const import WGS84


def gen_helm_aux(pt1,pt2):
    '''aux function for helmert values inversion.'''
    pt1 = pt1.astype(float)
    pt2 = pt2.astype(float)
    n_points=pt1.shape[0]

    unity_blk = _np.tile(_np.eye(3),reps=n_points).T

    xyz_blk = _np.zeros((n_points,3,3))
    xyz_blk[:,1,2] = pt1[:,0]  #x[1,2]
    xyz_blk[:,2,1] = -pt1[:,0] #x[2,1]

    xyz_blk[:,2,0] = pt1[:,1]  #y[2,0]
    xyz_blk[:,0,2] = -pt1[:,1] #y[0,2]

    xyz_blk[:,0,1] = pt1[:,2]  #z[0,1]
    xyz_blk[:,1,0] = -pt1[:,2] #z[1,0]

    xyz = pt1.reshape((n_points*3,1))
    A = _np.column_stack([unity_blk,xyz_blk.reshape((n_points*3,3)),xyz]) #matrix
    rhs = pt2.reshape((n_points*3,1)) - xyz #right-hand side
    return A, rhs

def get_helmert7(pt1,pt2):
    '''inversion of 7 Helmert parameters between 2 sets of points'''
    A, rhs = gen_helm_aux(pt1,pt2)
    sol = _np.linalg.lstsq(A, rhs,rcond=-1) # parameters
    res = rhs - _np.dot(A, sol[0])
    # sol[0] = [Tx, Ty, Tz, Rx, Ry, Rz, μ]
    return sol,res.reshape(res.shape[0]//3,3)

def gen_rot_matrix(v):
    '''creates rotation matrix for transform7
    from a list of [Rx, Ry, Rz] as in Altamimi'''
    x, y, z = v
    mat = _np.empty((3,3),dtype=float)
    mat[0] = [ 0, -z,  y]
    mat[1] = [ z,  0, -x]
    mat[2] = [-y,  x,  0]
    return mat + _np.eye(3)

def transform7(xyz_in,helmert_list):
    '''transformation of xyz vector with 7 helmert parameters'''
    translation = helmert_list[0:3]
    rotation = gen_rot_matrix(helmert_list[3:6].flatten())
    scale = helmert_list[6]

    xyz_out = ((xyz_in @ rotation)*(1+scale) + translation.T)
    return xyz_out

def xyz2llh_larson(xyz_array,ellipsoid=WGS84,tolerance = 1e-10,deg=False):
    '''vectorized version of xyz2llh function as in Larson's gnssIR'''
    x_arr,y_arr,z_arr = xyz_array[:,0],xyz_array[:,1],xyz_array[:,2]
    llh_array = _np.empty_like(xyz_array)

    _r = (x_arr*x_arr+y_arr*y_arr)**(1/2)
    phi0 = _np.arctan((z_arr/_r)/(1-ellipsoid.ecc1sq))
    phi = _np.empty_like(phi0,dtype=_np.float_)
    error_mask = phi0!=_np.nan # quick init of mask with all True

    for __ in range(10): #10 iterations cap as per Larson
        # prime vertical radius of curvature
        _n = ellipsoid.semimaj/(1-ellipsoid.ecc1sq*_np.sin(phi0[error_mask])**2)**(1/2)
        hei = _r[error_mask]/_np.cos(phi0[error_mask])-_n
        phi[error_mask] = _np.arctan((z_arr[error_mask]/_r[error_mask])/\
            (1-ellipsoid.ecc1sq*_n/(_n+hei)))
        error_mask = _np.abs(phi-phi0) > tolerance
        if error_mask.sum() == 0: #if all Falls
            break
        phi0 = phi.copy()#need to copy here otherwise it's a pointer

    # lam = _np.arctan2(y_arr, x_arr)

    llh_array[:,0] = phi #phi
    llh_array[:,1] = _np.arctan2(y_arr, x_arr) # lam
    llh_array[:,2] = hei #hei
    if deg:
        llh_array[:,:2] = _np.rad2deg(llh_array[:,:2])
    return llh_array

def xyz2llh_heik(xyz_array: _np.ndarray, ellipsoid=WGS84, deg=False):
    '''Heikkinen, M. (1982) 
    This is exact transformation and is pretty fast
    Output
      phi: latitude rad
      lam: longitude rad
      hei: height meters
    '''
    x_arr,y_arr,z_arr = xyz_array[:,0],xyz_array[:,1],xyz_array[:,2]
    llh_array = _np.empty_like(xyz_array)

    z_sq = z_arr*z_arr
    r_sq = x_arr*x_arr + y_arr*y_arr
    _r = (r_sq)**(1/2)

    _f = 54 * ellipsoid.semiminsq * z_sq
    _g = r_sq + (1 - ellipsoid.ecc1sq)*z_sq - \
        ellipsoid.ecc1sq*(ellipsoid.semimajsq - ellipsoid.semiminsq)
    _c = ellipsoid.ecc1sq*ellipsoid.ecc1sq*_f*r_sq/(_g*_g*_g)
    _s = (1 + _c + (_c*_c + _c + _c)**(1/2))**(1/3)
    _p = _f/(3*(_s + 1/_s + 1)**2*(_g*_g))
    _q = (1 + 2*(ellipsoid.ecc1sq*ellipsoid.ecc1sq*_p))**(1/2)
    r_0 = -(_p*ellipsoid.ecc1sq*_r)/(1+_q) + (ellipsoid.semimajsq/2*(1+1/_q)\
         - _p*(1 - ellipsoid.ecc1sq)*(z_sq)/(_q*(1+_q)) - _p*r_sq/2)**(1/2)

    r_ecc1sq_r0_sq = (_r - ellipsoid.ecc1sq*r_0)**2
    _u = (r_ecc1sq_r0_sq + z_sq)**(1/2)
    _v = (r_ecc1sq_r0_sq + (1-ellipsoid.ecc1sq)*z_sq)**(1/2)

    bsq_av = ellipsoid.semiminsq/(ellipsoid.semimaj*_v)
    z_0 = bsq_av*z_arr

    llh_array[:,0] = _np.arctan((z_arr+ellipsoid.ecc2sq*z_0)/_r) #phi
    llh_array[:,1] = _np.arctan2(y_arr, x_arr) # lam
    llh_array[:,2] = _u*(1 - bsq_av) #hei
    if deg:
        llh_array[:,:2] = _np.rad2deg(llh_array[:,:2])
    return llh_array

def xyz2llh_zhu(xyz_array,ellipsoid=WGS84,deg=False):
    '''Zhu, J. (1993)
    Output
      phi: latitude rad
      lam: longitude rad
      hei: height meters
    '''
    x_arr,y_arr,z_arr = xyz_array[:,0],xyz_array[:,1],xyz_array[:,2]
    llh_array = _np.empty_like(xyz_array)

    _l=ellipsoid.ecc1sq/2
    l_sq = _l*_l
    r_sq = x_arr*x_arr + y_arr*y_arr
    _r = r_sq**(1/2)
    _m = r_sq/ellipsoid.semimajsq
    ec1sq_z = (1-ellipsoid.ecc1sq)*z_arr
    _n = (ec1sq_z/ellipsoid.semimin)**2
    _i = -(2*l_sq + _m + _n)/2
    _k = l_sq*(l_sq - _m - _n)
    _q = (_m + _n - 4*l_sq)**3/216 + _m*_n*l_sq
    _d = ((2*_q - _m*_n*l_sq)*_m*_n*l_sq)**(1/2)

    beta = _i/3 - (_q+_d)**(1/3) - (_q-_d)**(1/3)
    _t = ((beta*beta - _k)**(1/2) - (beta+_i)/2)**(1/2) - _np.sign(_m-_n) * ((beta - _i)/2)**(1/2)

    r_0 = _r/(_t+_l)
    z_0 = ec1sq_z/(_t-_l)

    llh_array[:,0] = _np.arctan(z_0/((1 - ellipsoid.ecc1sq)*r_0)) #phi
    llh_array[:,1] = _np.arctan2(y_arr, x_arr) # lam
    llh_array[:,2] = _np.sign(_t - 1 + _l) * ((_r - r_0)**2 + (z_arr - z_0)**2)**(1/2) #hei
    if deg:
        llh_array[:,:2] = _np.rad2deg(llh_array[:,:2])
    return llh_array

def llh2xyz(lat,lon,hei,ellipsoid):
    '''Converts lat, lon and height to XYZ
    phi is geodetic latitude
    lam is geodetic longitude
    hei is the altitude normal to ellipsoid'''
    cos_phi = _np.cos(lat)
    sin_phi = _np.sin(lat)
    _rp = ellipsoid.semimaj/(1 - ellipsoid.ecc1sq*sin_phi*sin_phi)**(1/2)
    rp_h = _rp + hei

    x_arr = rp_h * cos_phi * _np.cos(lon)
    y_arr = rp_h * cos_phi * _np.sin(lon)
    z_arr = (rp_h - ellipsoid.ecc1sq*_rp) * sin_phi
    return x_arr,y_arr,z_arr


def llh2rot(phi, lamb):
    '''Creates R rotation matrices for n sites stacked
    on the 3d dimension from phi (lat) and lamb (lon).'''
    sin_lamb = _np.sin(lamb)
    cos_lamb = _np.cos(lamb)
    sin_phi  = _np.sin(phi)
    cos_phi  = _np.cos(phi)

    rot = _np.zeros((phi.shape[0],3,3),dtype=_np.float_)
    rot[:,0,0] =-sin_lamb
    rot[:,0,1] = cos_lamb

    rot[:,1,0] =-sin_phi*cos_lamb
    rot[:,1,1] =-sin_phi*sin_lamb
    rot[:,1,2] = cos_phi

    rot[:,2,0] = cos_phi*cos_lamb
    rot[:,2,1] = cos_phi*sin_lamb
    rot[:,2,2] = sin_phi
    return rot


def norm(a:_np.ndarray,axis:int=1)->_np.ndarray:
    '''Computes norm of every vector in the input array'''
    return _np.sqrt((a * a).sum(axis=axis))

def icrf2rac(a):

    #radial
    r = a.EST[['X','Y','Z']].values
    r_norm = norm(r)[_np.newaxis].T

    #along-track
    v = a.VELi[['X','Y','Z']].values/10000 #back to km/s
    v_norm = norm(v)[_np.newaxis].T

    #cross-track
    h = _np.cross(r,v)
    h_norm = norm(h)[_np.newaxis].T

    ea = v/v_norm
    ec = h/h_norm
    er = r/r_norm
    # er = _np.cross(ea,ec)
    matx = _np.dstack([er,ea,ec])
    
    RAC =  (r[:,_np.newaxis] * matx).sum(axis=1)
    return RAC
    # return _pd.DataFrame(RAC,index = a.index,columns=[['EST_RAC']*3,['R','A','C']])