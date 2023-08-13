
const vec2[2] S1 = vec2[2](vec2(200.0,200.0),vec2(600.0,200.0));
const vec2[2] S2 = vec2[2](vec2(900.0,300.0),vec2(600.0,200.0));

vec2 warp(vec2 p, vec2 center, float k) 
{
    k = 0.3 * k;
    vec2 d = p - center; 
    //return p + 300.0*exp(-dot(d,d)/(k*k)) * normalize(d);
    return p + 100.0 / (1.0 + abs(dot(d,d)) / k) * normalize(d);
}

vec2 cheapBend(in vec2 p)
{
    const float k = 0.001; // or some other amount
    float c = cos(k*p.x);
    float s = sin(k*p.x);
    mat2  m = mat2(c,-s,s,c);
    vec2  q = m*p;
    return q;
}

float distSeg(vec2 p, vec2 a, vec2 b) {
    //p = cheapBend(p);
    p = warp(p, a, 18000.0);
    //p = warp(p, b, 1.0/500.0);
    vec2 ab = b - a;
    vec2 ap = p - a;
    float side = sign(cross(vec3(ab,0.0),vec3(ap,0.0)).z);
    float d = dot(p - a, ab) / dot(ab,ab);
    d = clamp(d, 0.0, 1.0);
    vec2 p0 = a + d * ab;
    //float taper = max(0.0, 80.0 - distance(p,b)) / 80.0;
    return distance(p,p0);
}

float smin( float a, float b, float k )
{
    float h = max( k-abs(a-b), 0.0 )/k;
    return min( a, b ) - h*h*k*(1.0/4.0);
}

vec3 distVis(float d) {
    if (fract(d / 20.0) < 0.05) {
        return vec3(1.0, 1.0, 0.0);
    }
    else {
        return vec3(0.0);
    }
}


float hash( in ivec2 p )  // this hash is not production ready, please
{                         // replace this by something better

    // 2D -> 1D
    int n = p.x*3 + p.y*113;

    // 1D hash by Hugo Elias
    n = (n << 13) ^ n;
    n = n * (n * n * 15731 + 789221) + 1376312589;
    return -1.0+2.0*float( n & 0x0fffffff)/float(0x0fffffff);
}

float noise( vec2 p )
{
    ivec2 i = ivec2(floor( p ));
    vec2 f = fract( p );
    
    // quintic interpolant
    vec2 u = f*f*f*(f*(f*6.0-15.0)+10.0);

    return mix( mix( hash( i + ivec2(0,0) ), 
                     hash( i + ivec2(1,0) ), u.x),
                mix( hash( i + ivec2(0,1) ), 
                     hash( i + ivec2(1,1) ), u.x), u.y);
}

float fbm(vec2 uv) 
{
    float f= 0.0;
    uv /= 32.0;
    mat2 m = mat2( 1.6,  1.2, -1.2,  1.6 );
    f  = 0.5000*noise( uv ); uv = m*uv;
    //f += 0.2500*noise( uv ); uv = m*uv;
    //f += 0.1250*noise( uv ); uv = m*uv;
    //f += 0.0625*noise( uv ); uv = m*uv;
    return f;
}

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    // Normalized pixel coordinates (from 0 to 1)
    vec2 uv = fragCoord/iResolution.xy;
    
    vec2 pos0 = vec2(iMouse.xy);
    
    float dist = distSeg(fragCoord, pos0, S1[1]);
    float dist2 = distSeg(fragCoord, S2[0], S2[1]);
    dist = smin(dist,dist2,10.0);
    
    // stroke-aligned fragCoord
    vec2 u0 = normalize(S1[1]-pos0);
    vec2 u1 = vec2(-u0.y, u0.x);
    vec2 noiseCoord = vec2(dot(fragCoord-S1[1],u0), dot(fragCoord-S1[1],u1));
    
    float aastep = 0.7*fwidth(dist);
    dist += aastep * 5.0 * (fbm(noiseCoord) + 0.3*fbm(noiseCoord * 54.0));
    float thr = smoothstep(20.0-aastep, 20.0+aastep, dist); 

    // Output to screen
    fragColor = vec4(vec3(thr)/*-distVis(dist)*/,1.0);
}