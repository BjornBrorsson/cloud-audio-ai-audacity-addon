#include "ModCloudAI.h"
#include "CloudAIEffect.h"
#include <wx/log.h>

// Module constructor
ModCloudAI::ModCloudAI()
{
   wxLogMessage("Cloud AI Module: Initializing...");
}

ModCloudAI::~ModCloudAI()
{
   wxLogMessage("Cloud AI Module: Terminating...");
}

ComponentInterfaceSymbol ModCloudAI::GetSymbol() const
{
   return ComponentInterfaceSymbol{ XO("Cloud AI") };
}

VendorSymbol ModCloudAI::GetVendor() const
{
   return VendorSymbol{ XO("Bjorn Brorsson") };
}

wxString ModCloudAI::GetPath() const
{
   return wxT("CloudAI");
}

wxString ModCloudAI::GetDescription() const
{
   return XO("AI-powered audio generation and processing using ElevenLabs").Translation();
}

wxString ModCloudAI::GetVersion() const
{
   return wxT("1.0.0");
}

bool ModCloudAI::Initialize()
{
   wxLogMessage("Cloud AI Module: Initialize called");
   return RegisterEffects();
}

void ModCloudAI::Terminate()
{
   wxLogMessage("Cloud AI Module: Terminate called");
}

EffectFamilySymbol ModCloudAI::GetOptionalFamilySymbol()
{
   return EffectFamilySymbol{ XO("CloudAI") };
}

const FileExtensions &ModCloudAI::GetFileExtensions()
{
   static FileExtensions empty;
   return empty;
}

bool ModCloudAI::AutoRegisterPlugins(PluginManagerInterface &pm)
{
   wxLogMessage("Cloud AI Module: AutoRegisterPlugins called");
   return true;
}

PluginPaths ModCloudAI::FindModulePaths(PluginManagerInterface &pm)
{
   return {};
}

unsigned ModCloudAI::DiscoverPluginsAtPath(
   const PluginPath &path, TranslatableString &errMsg,
   const RegistrationCallback &callback)
{
   return 0;
}

bool ModCloudAI::CheckPluginExist(const PluginPath &path) const
{
   return false;
}

std::unique_ptr<ComponentInterface>
ModCloudAI::LoadPlugin(const PluginPath &path)
{
   return nullptr;
}

bool ModCloudAI::RegisterEffects()
{
   // Register all our AI effects
   wxLogMessage("Cloud AI Module: Registering effects...");
   
   // Effects will be auto-registered through Audacity's plugin system
   // We create them in CloudAIEffect.cpp
   
   return true;
}

// Module entry point
DEFINE_MODULE_ENTRY(ModCloudAI)
{
   return std::make_unique<ModCloudAI>();
}
