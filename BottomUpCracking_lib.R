load("Et.Model.RData")

#Fatigue Model: from MEPDG
Nf_Model_1=function(load.vec)
{
  v.a=7   # effective binder content (%)
  v.b=5   # air voids (%)
  C=10^(4.84*(v.b/(v.a+v.b)-0.69))
  
  beta1=1
  beta2=1
  beta3=1  # beta1,2,3 representing calibrating parameters of national pavements in MEPDG
 
  k1=0.007566
  k2=3.9492
  k3=1.281  # k1,2,3 representing regression coefficients
  
  E=6000*145.0377    # Modulus of materias, 6000 mpa converted to psi
  Et.vec=predict(poly2,newdata=data.frame(load=load.vec))
  Et.vec=Et.vec*(10^-6)  
  
  Nf.vec=0.00432*C*beta1*k1*(1/Et.vec)^(beta2*k2)*(1/E)^(beta3*k3)
  return(Nf.vec)
}

#Bottome Up Cracking Model
Crack_Model=function(D.vec)
{
  h.ac=6 # totoal thickness of asphalt layers
  c1=1
  c2=1  # c1,c2 set by MEPDF
  c2.new=-2.40874-39.748*(1+h.ac)^(-2.85609)
  c1.new=-2*c2.new
  crack=100/(1+exp(c1*c1.new-c2*c2.new*log10(D.vec*100)))
  return(crack)
}

#Fatigue Model: from Page 3.3.68 at http://onlinepubs.trb.org/onlinepubs/archive/mepdg/Part3_Chapter3_FlexibleDesign.pdf
Nf_Model_2=function(load.vec)
{
  v.a=7   # effective binder content (%)
  v.b=5   # air voids (%)
  C=10^(4.84*(v.b/(v.a+v.b)-0.69))
  h.ac=6 # totoal thickness of asphalt layers
  
  k1.prime=1/(0.000398+0.003602/(1+exp(11.02-3.49*h.ac)))
  
  E=6000*145.0377    # Modulus of materias, 6000 mpa converted to psi
  Et.vec=predict(poly2,newdata=data.frame(load=load.vec))
  Et.vec=Et.vec*(10^-6)  
  
  Nf.vec=0.00432*C*k1.prime*(1/Et.vec)^3.9492*(1/E)^1.281
  return(Nf.vec)
}

# Main Function of simulating bottom-up cracking


###