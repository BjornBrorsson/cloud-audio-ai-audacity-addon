#ifndef MOD_CLOUD_AI_H
#define MOD_CLOUD_AI_H

#include "ModuleInterface.h"
#include <wx/string.h>

class ModCloudAI final : public ModuleInterface
{
public:
   ModCloudAI();
   virtual ~ModCloudAI();

   // Module identification
   ComponentInterfaceSymbol GetSymbol() const override;
   VendorSymbol GetVendor() const override;
   wxString GetPath() const override;
   wxString GetDescription() const override;
   wxString GetVersion() const override;

   // Module registration
   bool Initialize() override;
   void Terminate() override;

   // Effect registration
   EffectFamilySymbol GetOptionalFamilySymbol() override;

   const FileExtensions &GetFileExtensions() override;
   FilePath InstallPath() override { return {}; }

   bool AutoRegisterPlugins(PluginManagerInterface &pm) override;
   PluginPaths FindModulePaths(PluginManagerInterface &pm) override;
   unsigned DiscoverPluginsAtPath(
      const PluginPath &path, TranslatableString &errMsg,
      const RegistrationCallback &callback) override;

   bool CheckPluginExist(const PluginPath &path) const override;

   std::unique_ptr<ComponentInterface>
   LoadPlugin(const PluginPath &path) override;

private:
   bool RegisterEffects();
};

// Module entry point
extern "C" {
   DECLARE_MODULE_ENTRY(ModCloudAI)
}

#endif // MOD_CLOUD_AI_H
